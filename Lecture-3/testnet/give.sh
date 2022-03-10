~/Cardano/cardano-node-1.33.0/cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 01.addr) \
    --tx-in 0758f77442aa52f96c960735434398f0c70371f3bab3e8a20283019b3d857758#0 \
    --tx-out "$(cat vesting.addr) 200000000 lovelace" \
    --tx-out-datum-hash-file unit.json \
    --out-file tx.body

~/Cardano/cardano-node-1.33.0/cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

~/Cardano/cardano-node-1.33.0/cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
