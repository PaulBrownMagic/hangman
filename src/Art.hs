module Art where

drawTitle :: IO ()
drawTitle = do
  putStrLn "   _"
  putStrLn "  | |"
  putStrLn "  | |__   __ _ _ __   __ _ _ __ ___   __ _ _ __"
  putStrLn "  | '_ \\ / _` | '_ \\ / _` | '_ ` _ \\ / _` | '_ \\ "
  putStrLn "  | | | | (_| | | | | (_| | | | | | | (_| | | | |"
  putStrLn "  |_| |_|\\__,_|_| |_|\\__, |_| |_| |_|\\__,_|_| |_|"
  putStrLn "                      __/ |"
  putStrLn "                     |___/"

drawHangman :: Int -> IO ()
drawHangman i = case i of
  0 -> do
      putStrLn "\n\n"
      putStrLn "                     ~"
      putStrLn "       ~\n\n"
      putStrLn "     ~\n\n\n\n\n\n\n\n\n\n\n\n\n"
      putStrLn ". .   \\|/          V  . ."
  1 -> do
      putStrLn "\n\n"
      putStrLn "                     ~"
      putStrLn "       ~\n\n"
      putStrLn "     ~\n\n\n\n\n\n\n\n\n\n"
      putStrLn "|\"|"
      putStrLn "| |"
      putStrLn ": :"
      putStrLn ". .   \\|/          V  . ."
  2 -> do
      putStrLn "\n\n"
      putStrLn "                     ~"
      putStrLn "       ~\n\n"
      putStrLn "     ~\n\n\n\n\n\n\n\n\n\n"
      putStrLn "|\"|                    |\"|"
      putStrLn "| |                    | |"
      putStrLn ": :                    : :"
      putStrLn ". .   \\|/          V  . ."
  3 -> do
      putStrLn "\n\n"
      putStrLn "                     ~"
      putStrLn "       ~\n\n"
      putStrLn "     ~\n\n\n\n\n\n\n\n\n"
      putStrLn "|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|"
      putStrLn "|\"|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|\"|"
      putStrLn "| |                    | |"
      putStrLn ": :                    : :"
      putStrLn ". .   \\|/          V  . ."
  4 -> do
      putStrLn ""
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |                  ~"
      putStrLn "| |    ~"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |  ~"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|"
      putStrLn "|\"|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|\"|"
      putStrLn "| |                    | |"
      putStrLn ": :                    : :"
      putStrLn ". .   \\|/          V  . ."
  5 -> do
      putStrLn " ____________________"
      putStrLn "| .__________________|"
      putStrLn "| |"
      putStrLn "| |                  ~"
      putStrLn "| |    ~"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |  ~"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|"
      putStrLn "|\"|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|\"|"
      putStrLn "| |                    | |"
      putStrLn ": :                    : :"
      putStrLn ". .   \\|/          V  . ."
  6 -> do
      putStrLn " ____________________"
      putStrLn "| .__________________|"
      putStrLn "| | / /"
      putStrLn "| |/ /               ~"
      putStrLn "| | /  ~"
      putStrLn "| |/"
      putStrLn "| |"
      putStrLn "| |  ~"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|"
      putStrLn "|\"|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|\"|"
      putStrLn "| |                    | |"
      putStrLn ": :                    : :"
      putStrLn ". .   \\|/          V  . ."
  7 -> do
      putStrLn " ___________.._______"
      putStrLn "| .__________))______|"
      putStrLn "| | / /      ||"
      putStrLn "| |/ /       ||      ~"
      putStrLn "| | /  ~     ||"
      putStrLn "| |/         ||"
      putStrLn "| |         //\\\\"
      putStrLn "| |  ~     ||  ||"
      putStrLn "| |         \\\\//"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "| |"
      putStrLn "|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|"
      putStrLn "|\"|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|\"|"
      putStrLn "| |                    | |"
      putStrLn ": :                    : :"
      putStrLn ". .   \\|/          V  . ."
  8 -> do
      putStrLn " ___________.._______"
      putStrLn "| .__________))______|"
      putStrLn "| | / /      ||"
      putStrLn "| |/ /       |.-''. ~"
      putStrLn "| | /  ~     /   _ \\"
      putStrLn "| |/         ||  `/,|"
      putStrLn "| |          (\\\\`_.'"
      putStrLn "| |  ~      .-`--'."
      putStrLn "| |        /Y . . Y\\"
      putStrLn "| |       // |   | \\\\"
      putStrLn "| |      //  | . |  \\\\"
      putStrLn "| |     ')   |   |   (`"
      putStrLn "| |          ||'||"
      putStrLn "| |          || ||"
      putStrLn "| |          || ||"
      putStrLn "| |          || ||"
      putStrLn "| |         / | | \\"
      putStrLn "|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|"
      putStrLn "|\"|\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"|\"|"
      putStrLn "| |                    | |"
      putStrLn ": :                    : :"
      putStrLn ". .   \\|/          V  . ."
  _ -> do
      putStrLn " ___________.._______"
      putStrLn "| .__________))______|"
      putStrLn "| | / /      ||"
      putStrLn "| |/ /       ||      ~"
      putStrLn "| | /  ~     ||.-''."
      putStrLn "| |/         |/  _  \\"
      putStrLn "| |          ||  `/,|"
      putStrLn "| |  ~       (\\\\`_.'"
      putStrLn "| |         .-`--'."
      putStrLn "| |        /Y . . Y\\"
      putStrLn "| |       // |   | \\\\"
      putStrLn "| |      //  | . |  \\\\"
      putStrLn "| |     ')   |   |   (`"
      putStrLn "| |          ||'||"
      putStrLn "| |          || ||"
      putStrLn "| |          || ||"
      putStrLn "| |          || ||"
      putStrLn "| |         / | | \\"
      putStrLn "\"\"\"\"\"\"\"\"\"\"|_`-' `-' |\"\"\"|"
      putStrLn "|\"|\"\"\"\"\"\"\"\\ \\       '\"|\"|"
      putStrLn "| |        \\ \\        | |"
      putStrLn ": :         \\ \\       : :"
      putStrLn ". .   \\|/    `'    V  . ."
