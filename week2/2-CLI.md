https://www.youtube.com/watch?v=2MbzKzoBiak&list=PLNEK_Ejlx3x1-oF7NDy0MhXxG7k5O6ZOA&index=2&ab_channel=IOGAcademy
# starting a local cardano node
```
 cardano-node run \
   --topology path/to/mainnet-topology.json \
   --database-path path/to/db \
   --socket-path path/to/db/node.socket \
   --host-addr x.x.x.x \
   --port 3001 \
   --config path/to/mainnet-config.json
```
get the config here https://developers.cardano.org/docs/get-started/running-cardano/


## cardano-cli
creating an address:  
1. `cardano-cli address key-gen --verfication-key-file "vkey" --signing-key-file "skey"`  
2. `cardano-cli address build --payment-verification-key-file "vkey" --testnet-magic 2 --out-file "addr"` 

getting current tip of local chain:  
`cardano-cli query tip --testnet-magic 2`

getting utxos owned by address:  
`cardano-cli query utxo --address <address> --testnet-magic 2`
