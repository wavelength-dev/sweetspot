import RIO
import Spec.Business
import Spec.Http
import Spec.Price
import Test.Hspec

main :: IO ()
main = hspec $ businessLogicSpec >> httpSpec >> priceSpec
