{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ExtendedDefaultRules #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Attoparsec.ByteString.Char8 
import qualified Data.ByteString as BS
import Text.InterpolatedString.Perl6 (q)

-- library imports for testing
import Data.BasicBashConfig


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, fileTests, brokenTests]

unitTests = testGroup "Unit tests"
  [ testCase "Comments and Whitespace" $
      parseOnly basic_bash_config_line_parser "# ...\n   #sdf\n" 
      @?= Right Nothing 
  , testCase "Comments & TEST array" $
      parseOnly basic_bash_config_file_parser "# ...\n   #sdf\nTEST=(\n \"v1\"\n  \"v2\"\n)\n"
      @?= Right [BasicBashConfig {configName = "TEST", configLines = ["v1","v2"]}]
  , testCase "Multiple Arrays in a file" $ 
      parseOnly basic_bash_config_file_parser "# \n TA=(\n \"v1\"\n \"v2\"\n)\nTB=(\n\"v3:v4\"\n\"v5:v6\"\n)\n"
      @?= Right [BasicBashConfig {configName = "TA", configLines = ["v1","v2"]},
                 BasicBashConfig {configName = "TB", configLines = ["v3:v4","v5:v6"]}]
  ] 


fileTests = testGroup "File Tests"
      [ testCase "Full file example" $
        parseOnly basic_bash_config_file_parser nicFile
        @?= Right [BasicBashConfig {configName = "NIC_ADDRS", configLines = 
           ["eth1:1.4.42.128", "eth2:1.4.42.129","eth3:1.4.42.130","eth4:1.4.42.131"]}]
  , testCase "More values from a file" $
      parseOnly basic_bash_config_file_parser complexFile
    @?= Right [BasicBashConfig {configName = "CONFIG_FILE", configLines =
      ["::127.128.9.99:3333:udp:abc0", "::128.127.9.99:5555:udp:abc1",
       "10.11.12.13/12::::name:abc2","10.11.12.14/30::::name:abc4"] }]
    ]


nicFile :: BS.ByteString
nicFile = [q|
#
# comment
#

# hostname:/etc/network/filename.txt
## comment 
# \n\
# comment commment comment ! comment 123  
# comment ('key:value')
#
NIC_ADDRS=( 
		"eth1:1.4.42.128"
		"eth2:1.4.42.129"
		"eth3:1.4.42.130"
		"eth4:1.4.42.131"
)

|]

complexFile :: BS.ByteString
complexFile  = [q|
#
# Comment ('key:value:value:value:value:value')
#
CONFIG_FILE=(
    "::127.128.9.99:3333:udp:abc0" #  (:comment:) 
    "::128.127.9.99:5555:udp:abc1"      # :comment:
    "10.11.12.13/12::::name:abc2" # :(comment:) 
    "10.11.12.14/30::::name:abc4" 	# (:comment):
 )

|]

brokenTests = testGroup "Tests of Currently Broken Behavior"
    [ testCase "FIXME: Config Name ending in 2" $ 
      parseOnly basic_bash_config_file_parser brokenFile
       @?= Right []
    ]
brokenFile :: BS.ByteString
brokenFile = [q|
#
C2=(
    "blah:bar:4" 	# 
 )

|]

