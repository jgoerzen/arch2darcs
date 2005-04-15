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

getDarcsDir = 
    do isdarcswc <- doesDirectoryExist "_darcs"
       if isdarcswc
          then getCurrentDirectory
          else fail "CWD is not a Darcs directory"

initLogging =
    sequence_ $ map (\x -> updateGlobalLogger x (setLevel DEBUG))
              ["arch2darcs", 
               --"MissingH.Cmd", "MissingH.Cmd.safeSystem",
               --"MissingH.Cmd.pOpen3", "MissingH.Cmd.pOpen",
               "main"]

info = infoM "main"

getLines cmd args func = 
    let f h = do c <- hGetLines h
                 seq c (func c)
        in pOpen ReadFromPipe cmd args f

initializeDarcs =
    do args <- getArgs
       case args of
          ["-i"] -> do info "Processing existing Arch situation..."
                       getLines "tla" ["logs", "-f"] (recordLog ["-l"] . last)
          _ -> return ()

main = do
       initLogging
       darcsdir <- getDarcsDir
       initializeDarcs
       info "Looking for new patches..."
       getLines "tla" ["missing", "-f"] (mapM_ procPatch)
       info "Done."

procPatch patchname =
    do info $ "Processing " ++ patchname
       getLines "tla" ["replay", patchname] handleReplay
       recordLog [] patchname

recordLog extraargs patchname = 
    getLines "tla" ["cat-log", patchname] (record extraargs patchname)

handleReplay lines =
    let splitline line =
            let cmdtype = head line
                fn = drop 4 line
                in (cmdtype, fn)
        procline ('A', fn) = safeSystem "darcs" ["add", fn]
        procline ('M', _)  = return ()
        procline ('D', _)  = return ()
        procline ('-', _)  = return ()
        procline ('*', _)  = return ()
        procline ('C', fn) = fail $ "Conflict on replay in " ++ fn
        procline (x, fn)   = warningM "main" $ "Unknown replay code " ++ [x] ++
                               " for " ++ fn
        noArchMeta (_, fn) =
            (not $ startswith "{arch}/" fn) && 
            (not $ contains ".arch-ids/" fn)
        in mapM_ procline . filter noArchMeta . map splitline $ lines
        
record extraargs patchname loglines = 
    let (date, creator, summary, log) = parseLog loglines
        pipestr = date ++ "\n" ++ creator ++ "\n" ++ 
                    summary ++ "\n" ++ log ++ "\n" ++
                    "(" ++ patchname ++ ")\n"
        in pOpen WriteToPipe "darcs" (["record", "-a", "--pipe"] ++ extraargs)
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