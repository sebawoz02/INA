#!/bin/bash
chuck_quote=$(curl -s 'https://api.chucknorris.io/jokes/random' | jq '.value')
curl -s $(curl -s 'https://api.thecatapi.com/v1/images/search?format=json' --header 'Content-Type: application/json' --header 'x-api-key: live_dqr7gCetytHqxz8FMSx4bkCV2y7oFAlT21Ju5r3nGZ0O9dDSC7Xg96HLdlXnCiG6' | jq -r '.[0].url')> image.png
catimg -r 2 image.png
rm image.png
echo "$chuck_quote"
