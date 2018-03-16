This is some helper functionality for encoding and decoding data to and from `ByteString`s when writing `reflex` libraries.

It sets up encoding and decoding so that things with `Binary` instances can be done incrementally and that `ByteString`s can be dealt with via the same interface.

