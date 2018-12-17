cli
---



### Command line

The `mismi-cli` module provides a command-line tool for interacting with s3 resources

```
cd mismi-cli
./mafia build
alias s3="dist/build/s3/s3"

s3 --help

Available commands:
  upload                   Upload a file to s3.
  download                 Download a file from s3.
  copy                     Copy a file from an S3 address to another S3 address.
  move                     Move an S3 address to another S3 address
  exists                   Check if an address exists.
  delete                   Delete an address.
  write                    Write to an address.
  read                     Read from an address.
  cat                      Stream data from an address.
  size                     Get the size of an address.
  sync                     Sync between two prefixes.
  ls                       Stream a recursively list of objects on a prefix
```
