package GlobalAuxiliaries

import v2.GlobalAuxiliaries.*
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class IdGenerator extends munit.FunSuite{
  test("generation"){
    initializeIdGenerator()
    def generate(n:Int) = (1 to 10).map(_=>generateId())
    assertEquals(generate(10), 1 to 10)
  }
}
