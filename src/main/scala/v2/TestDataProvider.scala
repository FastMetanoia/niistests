package v2

import v2.GlobalAuxiliaries.generateSystemModel
import v2.SystemModel.SystemModelProps

object TestDataProvider:
  def makeDataGenerator(
                                startProps:SystemModelProps,
                                fNextProps:SystemModelProps=>SystemModelProps
                              ):LazyList[SystemModel] =
    def go(
            startProps:SystemModelProps,
            fNextProps:SystemModelProps=>SystemModelProps
          ):LazyList[SystemModelProps] = startProps#::go(fNextProps(startProps),fNextProps)

    go(startProps,fNextProps)
      .map(generateSystemModel)

