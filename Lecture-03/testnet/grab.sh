cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 02.addr) \
    --tx-in d5fd871e73b8384b6d6c8f6d99587e0aa01d61ca8f1912faf6017acbeb968621#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --tx-in-collateral 0758f77442aa52f96c960735434398f0c70371f3bab3e8a20283019b3d857758#1 \
    --required-signer-hash f94c0cec5d8109761e47c05d4033847d0c284464cd69c03b3ee8426a \
    --invalid-before 52652761 \
    --protocol-params-file protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed
