import RIO
import Spec.Business
import Spec.Http
import Test.Hspec

main :: IO ()
main = hspec $ businessLogicSpec >> httpSpec
