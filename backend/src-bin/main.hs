import Backend (backend)
import Frontend (frontend)
import Obelisk.Backend (runBackend)

main :: IO ()
main = runBackend backend frontend
