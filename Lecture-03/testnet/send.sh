~/Cardano/cardano-node-1.33.0/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic    1097911063 \
    --change-address   $(cat 01.addr) \
    --tx-in            a6616aa833fb6f8858fb1bf788d14ef7c4f4ad8569fc1d384ed2bec9c7f6aed4#0 \
    --tx-out           "$(cat 02.addr) 10000000 lovelace" \
    --out-file         tx.body

~/Cardano/cardano-node-1.33.0/cardano-cli transaction sign \
    --tx-body-file     tx.body \
    --signing-key-file 01.skey \
    --testnet-magic    1097911063 \
    --out-file         tx.signed

~/Cardano/cardano-node-1.33.0/cardano-cli transaction submit \
    --testnet-magic    1097911063 \
    --tx-file          tx.signed
