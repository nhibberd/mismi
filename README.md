mismi
=====

[![Build Status](https://travis-ci.org/nhibberd/mismi.svg?branch=master)](https://travis-ci.org/nhibberd/mismi)

<img src="http://upload.wikimedia.org/wikipedia/commons/a/a4/Nevado_Mismi.jpg" width="307" align="right"/>

> Source of the Amazon
> - http://en.wikipedia.org/wiki/Amazon_River

`mismi` is a haskell library built on top of
[amazonka](https://github.com/brendanhay/amazonka) that is designed as
a robust, higher level of abstraction to AWS interfaces. Sub-packages
within mismi are built individually per AWS service to aid with
haskell modularity.

This library has been used extensively in a production setting at
[`ambiata`](https://github.com/ambiata) since early 2015.


## Release

The goal of the initial hackage release `0.0.1` was to provide mismi
as is, with minimal change. Future work will look at removing a lot of
the technical debt that has accumlated over the years will most like
result in a major version change.

- [mismi-p](https://hackage.haskell.org/package/mismi-p-0.0.1)
- [mismi-kernel](https://hackage.haskell.org/package/mismi-kernel-0.0.1)
- [mismi-core](https://hackage.haskell.org/package/mismi-core-0.0.1)


## AWS Testing

### Permissions

Permissions required for testing:

- _tba_


### Running tests

- `AWS_TEST`

Optional environment variables that can be used to disable testing on
AWS resources, all variables default to 'true'.

- `AWS_TEST_AUTOSCALING`
- `AWS_TEST_S3`

### Resources

Resources that are used when running test are configurable via:

```
`AWS_TEST_SECURITY_GROUP` - defaults to "ci.ci.node"
`AWS_TEST_IMAGE_ID` - defaults to "ami-a1abbfc2"
`AWS_TEST_IAM_ROLE` - defaults to "ci.ci.node"
`AWS_TEST_BUCKET` - defaults to "ambiata-dev-view"
```

## Debugging

### Amazonka - environment variable

Set `AWS_DEBUG` to `true` to enable amazonka debugging

### Amazonka - manually

See `Mismi.Control` to add a logger to the runner
```
runAWS :: Region -> AWS a -> EitherT AWSError IO a
runAWS r a = do
  lgr <- newLogger Trace stdout
  e <- liftIO $ AWS.getEnv r Discover <&> envLogger .~ lgr
  runAWSWithEnv e a
```
