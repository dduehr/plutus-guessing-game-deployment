# Plutus Guessing Game Deployment

[![Haskell](https://img.shields.io/badge/Haskell-8.10.7-5b4c83)](https://www.haskell.org/)
[![Plutus Apps](https://img.shields.io/badge/Plutus%20Apps-v1.1.0-blue)](https://github.com/input-output-hk/plutus-apps)

This is a small accompanying project for the [Cardano DApp Web-Frontend](https://github.com/dduehr/purescript-cardano-dapp-webfrontend).

It creates a [Cardano](https://cardano.org/) blockchain address and CBOR representation of the 
[IOG Guessing Game](https://github.com/input-output-hk/plutus-apps/blob/v1.1.0/plutus-use-cases/src/Plutus/Contracts/Game.hs).
The game is part of the package [plutus-use-cases](https://github.com/input-output-hk/plutus-apps/tree/v1.1.0/plutus-use-cases) of the IOG 
[plutus-apps](https://github.com/input-output-hk/plutus-apps) repository.

The address and CBOR representation of the game can be used for the use-cases  *"Send ADA to smart contract"* and *"Redeem ADA from smart contract"*
of the above dApp web-frontend.

## Build

The project is build within a `nix-shell` established in the root directory of the IOG `plutus-apps`
[repository](https://github.com/input-output-hk/plutus-apps). 

Install [Nix](https://nixos.org/nix/) unless already done, checkout the git tag `v1.1.0` (2023-01-12) of the `plutus-apps` repository
and build the Haskell packages and other artifacts as described by 
[*"How to build the project’s artifacts"*](https://github.com/input-output-hk/plutus-apps#how-to-build-the-projects-artifacts):

```
git clone https://github.com/input-output-hk/plutus-apps.git
cd plutus-apps
git checkout v1.1.0
nix build -f default.nix plutus-apps.haskell.packages.plutus-pab.components.library
```

Finally start a `nix-shell` in the `plutus-apps` directory and switch to the directory of this project in order
to build the latter with [Cabal](https://www.haskell.org/cabal/):

```
nix-shell
cd ../plutus-guessing-game-deployment
cabal update
cabal build
```

See the file [cabal.project](https://github.com/dduehr/plutus-guessing-game-deployment/cabal.project#L99) to optionally configure the dependency
of this project to the tag or commit hash of the `plutus-apps` repository.

## Run

Run the application to create the address and CBOR representation of the IOG guessing game:

Optionally change the customization address for the IOG Guessing Game, it's start time and network id (e.g. preview or pre-production testnet),
as defined in the source file [Main.hs](https://github.com/dduehr/plutus-guessing-game-deployment/app/Main.hs#L52) beforehand.

```
cabal run
```

Example of a created script address (Bech32):

```
addr_test1wr4qwdxvp3fmh5rfy7kc6yfje7crg84d3kwz5nfquyq7dgqjcypkq
```

The corresponding script representation (CBOR):

```
5907e65907e3010000332323232323232323232323232323322323232322222323253353330063357389211944617461206465636f646564207375636365737366756c6c7900375c00a66ae712411d52656465656d6572206465636f646564207375636365737366756c6c7900375c00866ae712412353637269707420636f6e74657874206465636f646564207375636365737366756c6c79003333573466e1cd55cea8012400046644246600200600464646464646464646464646666ae68cdc39aab9d500a480008cccccccccc888888888848cccccccccc00402c02802402001c01801401000c008cd40508c8c8cccd5cd19b8735573aa0049000119910919800801801180f9aba150023019357426ae8940088c98c80a0cd5ce01501401309aab9e5001137540026ae854028cd4050054d5d0a804999aa80bbae501635742a010666aa02eeb94058d5d0a80399a80a00f9aba15006335014335502202075a6ae854014c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502575a6ae854008c098d5d09aba2500223263202c33573805c05805426aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a04aeb4d5d0a80118131aba135744a004464c6405866ae700b80b00a84d55cf280089baa001357426ae8940088c98c80a0cd5ce01501401309aab9e5001137540026ae854010cd4051d71aba15003335014335502275c40026ae854008c070d5d09aba2500223263202433573804c04804426ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226aae7940044dd50009aba150023232323333573466e1d400520062321222230040053017357426aae79400c8cccd5cd19b875002480108c848888c008014c064d5d09aab9e500423333573466e1d400d20022321222230010053015357426aae7940148cccd5cd19b875004480008c848888c00c014dd71aba135573ca00c464c6403e66ae7008407c07407006c0684d55cea80089baa001357426ae8940088c98c8060cd5ce00d00c00b080b89931900b99ab9c4910350543500017135573ca00226ea800448c88c008dd6000990009aa80a111999aab9f00125009233500830043574200460066ae8800804c8c8c8c8cccd5cd19b8735573aa00690001199911091998008020018011919191999ab9a3370e6aae7540092000233221233001003002301535742a00466a01c0286ae84d5d1280111931900c19ab9c01a018016135573ca00226ea8004d5d0a801999aa803bae500635742a00466a014eb8d5d09aba2500223263201433573802c02802426ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80911191999aab9f0022500823350073355014300635573aa004600a6aae794008c010d5d100180909aba100111220021221223300100400312232323333573466e1d4005200023212230020033005357426aae79400c8cccd5cd19b8750024800884880048c98c8040cd5ce00900800700689aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8034cd5ce00780680589aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401666ae7003402c0244dd500089119191999ab9a3370ea00290021091100091999ab9a3370ea00490011190911180180218031aba135573ca00846666ae68cdc3a801a400042444004464c6401c66ae7004003803002c0284d55cea80089baa0012323333573466e1d40052002212200223333573466e1d40092000212200123263200a33573801801401000e26aae74dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402466ae7005004804003c03803403002c0284d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200b33573801a01601201026aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900419ab9c00a008006005135573aa00226ea800444888c8c8cccd5cd19b8735573aa0049000119aa80498031aba150023005357426ae8940088c98c8020cd5ce00500400309aab9e500113754002930900088910919800801801248103505431001123230010012233003300200200132222333573466e3c00cdc900109100109100099a89119801244139004b83f2126602db14ed7ca42e249c20ac33d063cfa031ba02d2d1978de503234ad669debc89aadb50ce2db7c7263e9baa4406547220e3c26a00480008848cc00400c0088005
```
