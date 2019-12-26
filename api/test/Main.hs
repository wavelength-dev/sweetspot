import Test.Hspec
import Spec.Business
import Spec.Http

main :: IO ()
main = hspec $ businessLogicSpec >> httpSpec
