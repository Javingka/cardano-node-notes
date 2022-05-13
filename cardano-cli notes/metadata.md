
https://github.com/input-output-hk/cardano-node/blob/master/doc/reference/tx-metadata.md 

cardano-cli transaction build-raw --tx-in cecf980019dcdc4edfe9d0a5c38bb0d9ac647bfd5a0d7492ef6e462972b067eb#0 --tx-out $(cat payment2.addr)+100000 --tx-out $(cat payment1.addr)+0 --metadata-json-file metadata.json --fee 0 --out-file metatx2.draft

 cardano-cli transaction calculate-min-fee --tx-body-file metatx2.draft --tx-in-count 1 --tx-out-count 2 --witness-count 1 $TESTNET --protocol-params-file protocol.json 

fee: 178481

cardano-cli transaction build-raw --tx-in cecf980019dcdc4edfe9d0a5c38bb0d9ac647bfd5a0d7492ef6e462972b067eb#0 --tx-out $(cat payment1.addr)+100000 --tx-out $(cat payment2.addr)+249721519 --metadata-json-file metadata.json --fee 178481 --out-file metatx2.unsigned

acabei fazendo seguindo a pagina e fazendo alguns ajusts:
https://developers.cardano.org/docs/transaction-metadata/how-to-create-a-metadata-transaction-cli/


cardano-cli address key-gen \
> --verification-key-file payment.vkey \
> --signing-key-file payment.skey

cardano-cli address build \
> --payment-verification-key-file payment.vkey \
> --out-file payment.addr $TESTNET

#verify balance
cardano-cli query utxo --testnet-magic 1097911063 --address $(cat payment.addr)

javingka@javingka-B450-AORUS-ELITE:~/cardano-src/keys/test02$ cardano-cli transaction build-raw --tx-in dc82699918c6dd539ed67a8bd535c1192f3ebccfb5db14e551a91c118ffc2f84#0 --tx-out $(cat payment1.addr)+100000000 --tx-out $(cat payment.addr)+0 --metadata-json-file metadata.json --fee 0 --out-file metatx.draft


javingka@javingka-B450-AORUS-ELITE:~/cardano-src/keys/test02$ cardano-cli transaction calculate-min-fee --tx-body-file metatx.draft --tx-in-count 1 --tx-out-count 1 --witness-count 1 --byron-witness-count 0 $TESTNET --protocol-params-file protocol.json
175621 Lovelace

javingka@javingka-B450-AORUS-ELITE:~/cardano-src/keys/test02$ cardano-cli transaction build-raw --tx-in dc82699918c6dd539ed67a8bd535c1192f3ebccfb5db14e551a91c118ffc2f84#0 --tx-out $(cat payment1.addr)+100000000 --tx-out $(cat payment.addr)+899824379 --metadata-json-file metadata.json --fee 175621 --out-file metatx.unsigned


javingka@javingka-B450-AORUS-ELITE:~/cardano-src/keys/test02$ cardano-cli transaction sign --tx-body-file metatx.unsigned --signing-key-file payment.skey $TESTNET --out-file metatx.signed 

javingka@javingka-B450-AORUS-ELITE:~/cardano-src/keys/test02$ cardano-cli transaction submit --tx-file metatx.signed $TESTNET

