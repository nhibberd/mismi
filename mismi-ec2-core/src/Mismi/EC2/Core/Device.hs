{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Mismi.EC2.Core.Device (
    instanceDeviceMappings
  ) where

import           Mismi.EC2.Core.Data

-- http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html#instance-store-volumes
instanceDeviceMappings :: MismiInstanceType -> [BlockDeviceMapping]

-- General Purpose
instanceDeviceMappings T1_Micro = devices0

instanceDeviceMappings T2_Nano = devices0
instanceDeviceMappings T2_Micro = devices0
instanceDeviceMappings T2_Small = devices0
instanceDeviceMappings T2_Medium = devices0
instanceDeviceMappings T2_Large = devices0
instanceDeviceMappings T2_2XLarge = devices0
instanceDeviceMappings T2_XLarge = devices0

instanceDeviceMappings M1_Small = devices1
instanceDeviceMappings M1_Medium = devices1
instanceDeviceMappings M1_Large = devices2
instanceDeviceMappings M1_XLarge = devices2

instanceDeviceMappings M3_Medium = devices1
instanceDeviceMappings M3_Large = devices1
instanceDeviceMappings M3_XLarge = devices2
instanceDeviceMappings M3_2XLarge = devices2

instanceDeviceMappings M4_10XLarge = devices0
instanceDeviceMappings M4_16XLarge = devices0
instanceDeviceMappings M4_2XLarge = devices0
instanceDeviceMappings M4_4XLarge = devices0
instanceDeviceMappings M4_XLarge = devices0
instanceDeviceMappings M4_Large = devices0

instanceDeviceMappings M5_12XLarge = devices0
instanceDeviceMappings M5_24XLarge = devices0
instanceDeviceMappings M5_2XLarge = devices0
instanceDeviceMappings M5_4XLarge = devices0
instanceDeviceMappings M5_Large = devices0
instanceDeviceMappings M5_XLarge = devices0


-- Compute Optimised
instanceDeviceMappings C1_Medium = devices1
instanceDeviceMappings C1_XLarge = devices4

instanceDeviceMappings C3_Large = devices2
instanceDeviceMappings C3_XLarge = devices2
instanceDeviceMappings C3_2XLarge = devices2
instanceDeviceMappings C3_4XLarge = devices2
instanceDeviceMappings C3_8XLarge = devices2

instanceDeviceMappings C4_Large = devices0
instanceDeviceMappings C4_XLarge = devices0
instanceDeviceMappings C4_2XLarge = devices0
instanceDeviceMappings C4_4XLarge = devices0
instanceDeviceMappings C4_8XLarge = devices0

instanceDeviceMappings C5_18XLarge = devices0
instanceDeviceMappings C5_2XLarge = devices0
instanceDeviceMappings C5_4XLarge = devices0
instanceDeviceMappings C5_9XLarge = devices0
instanceDeviceMappings C5_Large = devices0
instanceDeviceMappings C5_XLarge = devices0

-- Accelerated Compute
instanceDeviceMappings F1_2XLarge = devicesNVMe1
instanceDeviceMappings F1_16XLarge = devicesNVMe4

instanceDeviceMappings G2_2XLarge = devices1
instanceDeviceMappings G2_8XLarge = devices2

instanceDeviceMappings G3_16XLarge = devices0
instanceDeviceMappings G3_4XLarge = devices0
instanceDeviceMappings G3_8XLarge = devices0

instanceDeviceMappings P2_XLarge = devices0
instanceDeviceMappings P2_8XLarge = devices0
instanceDeviceMappings P2_16XLarge = devices0
instanceDeviceMappings P3_16XLarge = devices0
instanceDeviceMappings P3_2XLarge = devices0
instanceDeviceMappings P3_8XLarge = devices0

-- Memory Optimised
instanceDeviceMappings M2_XLarge = devices1
instanceDeviceMappings M2_2XLarge = devices1
instanceDeviceMappings M2_4XLarge = devices2

instanceDeviceMappings R3_Large = devices1
instanceDeviceMappings R3_XLarge = devices1
instanceDeviceMappings R3_2XLarge = devices1
instanceDeviceMappings R3_4XLarge = devices1
instanceDeviceMappings R3_8XLarge = devices2

instanceDeviceMappings R4_16XLarge = devices0
instanceDeviceMappings R4_2XLarge = devices0
instanceDeviceMappings R4_4XLarge = devices0
instanceDeviceMappings R4_8XLarge = devices0
instanceDeviceMappings R4_Large = devices0
instanceDeviceMappings R4_XLarge = devices0

instanceDeviceMappings X1_16XLarge = devices1
instanceDeviceMappings X1_32XLarge = devices2

instanceDeviceMappings X1e_16XLarge = devices2
instanceDeviceMappings X1e_2XLarge = devices1
instanceDeviceMappings X1e_32XLarge = devices1
instanceDeviceMappings X1e_4XLarge = devices1
instanceDeviceMappings X1e_8XLarge = devices1
instanceDeviceMappings X1e_XLarge = devices1

-- High Storage Density
instanceDeviceMappings HS1_8XLarge = devices24

-- Storage Optimised
instanceDeviceMappings I2_XLarge = devices1
instanceDeviceMappings I2_2XLarge = devices2
instanceDeviceMappings I2_4XLarge = devices4
instanceDeviceMappings I2_8XLarge = devices8

instanceDeviceMappings I3_16XLarge = devicesNVMe8
instanceDeviceMappings I3_2XLarge = devicesNVMe1
instanceDeviceMappings I3_4XLarge = devicesNVMe2
instanceDeviceMappings I3_8XLarge = devicesNVMe4
instanceDeviceMappings I3_Large = devicesNVMe1
instanceDeviceMappings I3_XLarge = devicesNVMe1

instanceDeviceMappings H1_2XLarge = devices1
instanceDeviceMappings H1_4XLarge = devices2
instanceDeviceMappings H1_8XLarge = devices4
instanceDeviceMappings H1_16XLarge = devices8

instanceDeviceMappings D2_2XLarge = devices6
instanceDeviceMappings D2_4XLarge = devices12
instanceDeviceMappings D2_8XLarge = devices24
instanceDeviceMappings D2_XLarge = devices3


-- Extra
instanceDeviceMappings CC1_4XLarge = devices0
instanceDeviceMappings CC2_8XLarge = devices4

instanceDeviceMappings CG1_4XLarge = devices2

instanceDeviceMappings CR1_8XLarge = devices2

instanceDeviceMappings HI1_4XLarge = devices2


devices0 :: [BlockDeviceMapping]
devices0 = []

devices1 :: [BlockDeviceMapping]
devices1 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  ]

devicesNVMe1 :: [BlockDeviceMapping]
devicesNVMe1 = [
    BlockDeviceMapping "/dev/nvme0n1" "ephemeral0"
  ]

devices2 :: [BlockDeviceMapping]
devices2 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  ]

devicesNVMe2 :: [BlockDeviceMapping]
devicesNVMe2 = [
    BlockDeviceMapping "/dev/nvme0n1" "ephemeral0"
  , BlockDeviceMapping "/dev/nvme1n1" "ephemeral1"
  ]

devices3 :: [BlockDeviceMapping]
devices3 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  ]

devices4 :: [BlockDeviceMapping]
devices4 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  ]

devicesNVMe4 :: [BlockDeviceMapping]
devicesNVMe4 = [
    BlockDeviceMapping "/dev/nvme0n1" "ephemeral0"
  , BlockDeviceMapping "/dev/nvme1n1" "ephemeral1"
  , BlockDeviceMapping "/dev/nvme2n1" "ephemeral2"
  , BlockDeviceMapping "/dev/nvme3n1" "ephemeral3"
  ]

devices6 :: [BlockDeviceMapping]
devices6 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  ]

devices8 :: [BlockDeviceMapping]
devices8 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  , BlockDeviceMapping "/dev/xvdh" "ephemeral6"
  , BlockDeviceMapping "/dev/xvdi" "ephemeral7"
  ]

devicesNVMe8 :: [BlockDeviceMapping]
devicesNVMe8 = [
    BlockDeviceMapping "/dev/nvme0n1" "ephemeral0"
  , BlockDeviceMapping "/dev/nvme1n1" "ephemeral1"
  , BlockDeviceMapping "/dev/nvme2n1" "ephemeral2"
  , BlockDeviceMapping "/dev/nvme3n1" "ephemeral3"
  , BlockDeviceMapping "/dev/nvme4n1" "ephemeral4"
  , BlockDeviceMapping "/dev/nvme5n1" "ephemeral5"
  , BlockDeviceMapping "/dev/nvme6n1" "ephemeral6"
  , BlockDeviceMapping "/dev/nvme7n1" "ephemeral7"
  ]

devices12 :: [BlockDeviceMapping]
devices12 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  , BlockDeviceMapping "/dev/xvdh" "ephemeral6"
  , BlockDeviceMapping "/dev/xvdi" "ephemeral7"
  , BlockDeviceMapping "/dev/xvdj" "ephemeral8"
  , BlockDeviceMapping "/dev/xvdk" "ephemeral9"
  , BlockDeviceMapping "/dev/xvdl" "ephemeral10"
  , BlockDeviceMapping "/dev/xvdm" "ephemeral11"
  ]

devices24 :: [BlockDeviceMapping]
devices24 = [
    BlockDeviceMapping "/dev/xvdb" "ephemeral0"
  , BlockDeviceMapping "/dev/xvdc" "ephemeral1"
  , BlockDeviceMapping "/dev/xvdd" "ephemeral2"
  , BlockDeviceMapping "/dev/xvde" "ephemeral3"
  , BlockDeviceMapping "/dev/xvdf" "ephemeral4"
  , BlockDeviceMapping "/dev/xvdg" "ephemeral5"
  , BlockDeviceMapping "/dev/xvdh" "ephemeral6"
  , BlockDeviceMapping "/dev/xvdi" "ephemeral7"
  , BlockDeviceMapping "/dev/xvdj" "ephemeral8"
  , BlockDeviceMapping "/dev/xvdk" "ephemeral9"
  , BlockDeviceMapping "/dev/xvdl" "ephemeral10"
  , BlockDeviceMapping "/dev/xvdm" "ephemeral11"
  , BlockDeviceMapping "/dev/xvdn" "ephemeral12"
  , BlockDeviceMapping "/dev/xvdo" "ephemeral13"
  , BlockDeviceMapping "/dev/xvdp" "ephemeral14"
  , BlockDeviceMapping "/dev/xvdq" "ephemeral15"
  , BlockDeviceMapping "/dev/xvdr" "ephemeral16"
  , BlockDeviceMapping "/dev/xvds" "ephemeral17"
  , BlockDeviceMapping "/dev/xvdt" "ephemeral18"
  , BlockDeviceMapping "/dev/xvdu" "ephemeral19"
  , BlockDeviceMapping "/dev/xvdv" "ephemeral20"
  , BlockDeviceMapping "/dev/xvdw" "ephemeral21"
  , BlockDeviceMapping "/dev/xvdx" "ephemeral22"
  , BlockDeviceMapping "/dev/xvdy" "ephemeral23"
  ]
