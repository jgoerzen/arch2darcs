{- 
Copyright (C) 2005 John Goerzen <jgoerzen@complete.org>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
-}

module Main where
import System.Environment
import System.Directory
import MissingH.Logging.Logger
import MissingH.IO
import MissingH.Cmd
import MissingH.List
import Text.ParserCombinators.Parsec
import System.IO
import Text.Regex
import MissingH.Str
import System.Posix.Files

usage = "Usage: arch2darcs [-i]\n" ++
 "\n" ++
 "You must be in your darcs working copy before running this command.\n" ++
 "\n" ++
 "Will apply any Arch patches, to the darcs repository in the current\n"++
 "working directory, that are not\n" ++
 "already in the current working directory.\n" ++
 "\n" ++
 "If you give -i, initialize the darcs repo using what is present in the\n" ++
 "directory currently."

initLogging =
    sequence_ $ map (\x -> updateGlobalLogger x (setLevel DEBUG))
              ["arch2darcs", "main"] --, "MissingH.Cmd.pOpen3", "MissingH.Cmd.safeSystem"]

info = infoM "main"

getLines cmd args func = 
    let f h = do c <- hGetLines h
                 seq c (func c)
        in pOpen ReadFromPipe cmd args f

initializeDarcs =
    do args <- getArgs
       case args of
          ["-i"] -> do info "Processing existing Arch situation..."
                       getLines "tla" ["logs", "-f"] (recordLog " -l" . last)
          [] -> return ()
          _ -> do putStr usage
                  fail "Invalid command-line syntax"

main = do
       initLogging
       initializeDarcs
       info "Looking for new patches..."
       getLines "tla" ["missing", "-f"] (mapM_ procPatch)
       info "Done."

procPatch patchname =
    do info $ "Processing " ++ patchname
       getLines "tla" ["replay", patchname] handleReplay
       recordLog "" patchname

recordLog extraargs patchname = 
    getLines "tla" ["cat-log", patchname] (record extraargs patchname)

handleReplay lines =
    let splitline line =
            let cmdtype = head line
                fn = drop 4 line
                in (cmdtype, fn)
        procline ('A', fn) = safeSystem "darcs" ["add", "--case-ok", fn]
        procline ('=', fn) = darcsRename (split "\t" fn)
        procline ('/', fn) = darcsRename (split "\t" fn)
        procline ('C', fn) = fail $ "Conflict on replay in " ++ fn
        procline (x, fn)
            | x `elem` "MD-*c" = return () -- Ignore these chars
            | otherwise = warningM "main" $ "Unknown replay code " ++ [x] ++
                            " for " ++ fn
        noArchMeta (_, fn) =
            (not $ startswith "{arch}/" fn) && 
            (not $ contains ".arch-ids" fn)
        in mapM_ procline . filter noArchMeta . map splitline $ lines

darcsRename [src, dest] = 
    let tmpname = ",,arch2darcs-tmp-rename"
        darcsmv = safeSystem "darcs" $ ["mv", "--case-ok", src, dest]
        in if src == dest || src == "./" ++ dest || "./" ++ src == dest
              then return ()
              else do f <- fileExist src
                      if f 
                      -- If the source file exists, darcs mv gives an error 
                      -- because the dest file is already there.  Temporarily 
                      -- hide the source file from darcs mv so there's no
                      -- error, then move it back.
                         then do rename src tmpname
                                 darcsmv
                                 rename tmpname src
                         else darcsmv
darcsRename x = fail $ "Bad rename line in replay: " ++ show x

record extraargs patchname loglines = 
    let (date, creator, summary, log) = parseLog loglines
        pipestr = date ++ "\n" ++ creator ++ "\n" ++ 
                    summary ++ "\n" ++ log ++ "\n" ++
                    "(" ++ patchname ++ ")\n"
        in pOpen WriteToPipe "sh"
               ["-c", "darcs record -a --pipe" ++ extraargs ++ " > /dev/null"]
               (\h -> hPutStr h pipestr)
              
parseLog loglines =
    let findline hdrname [] = error $ "Couldn't find " ++ hdrname
        findline hdrname (x:xs) =
            if startswith (hdrname ++ ": ") x
               then (drop (2 + length hdrname) x, xs)
               else findline hdrname xs
        (date, _) = findline "Standard-date" loglines
        (creator, _) = findline "Creator" loglines
        (summary, log) = findline "Summary" loglines
        darcsdate = subRe (mkRegex "[^0-9]") date ""
        in (darcsdate, creator, summary, unlines log)