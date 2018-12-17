s3
--

##### STS
The `"X-Amz-Security-Token"` header is only supported by amazonka and therefore all
required sts functionality will have to be implemented in terms of `amazonka-s3`.

Running amazonka `AWST` - [Mismi.Control.Amazonka](https://github.com/ambiata/mismi/blob/master/mismi-core/src/Mismi/Control/Amazonka.hs#L81)
```
runAWSWithCreds :: Region -> AccessKey -> SecretKey -> Maybe SecurityToken -> Maybe UTCTime -> AWS a -> EitherT AWSError IO a
```
