import RIO
import Spec.Business
import Spec.Http
import Spec.Pagination
import Spec.Price
import Test.Hspec

main :: IO ()
main = hspec $ businessLogicSpec >> httpSpec >> priceSpec >> paginationSpec
