module Controllers.Telegram.LogHelper where

import Logging (prependSections, LoggingFunctionBuilder)


logApi :: LoggingFunctionBuilder -> LoggingFunctionBuilder
logApi = prependSections ["API"]


logUpdate :: LoggingFunctionBuilder -> LoggingFunctionBuilder
logUpdate = prependSections ["telegram:update"] . logApi


logCommand :: LoggingFunctionBuilder -> LoggingFunctionBuilder
logCommand = prependSections ["TextMessage"] . logUpdate