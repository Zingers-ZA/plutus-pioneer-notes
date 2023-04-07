# Hashing and digital signatures
https://www.youtube.com/watch?v=f-WKPWbk9Jg&list=PLNEK_Ejlx3x3xFHJJKdyfo9eB0Iw-OQDd&index=4&ab_channel=IOGAcademy

## hashing:

### Algorithms: 
SHA-256: 32bit length hashing

- output of hashes will always be the same length
- no way to predict what output a given input will have
- practically impossible to reverse engineer
- most blockchains rely heavily on the fact that it is impossible to find the input text for a given hash

## Digital signatures:

### Algorithms: 
RSA
ECDSA

- allows proof of digitally signing documents
- requires a public and private key pair
- public key can be derived from the private key, but not the other way around
- given the signature, public key, and payload, the receiver can verify that the payload was signed by the public key
- signature is tied to payload
- signature is tied to public key

