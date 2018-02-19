{ pkgs ? import ./pkgs.nix }:

let
  purescript = pkgs.purescript;
in
pkgs.stdenv.mkDerivation {
  name = "purescript-datetime";
  src = ./.;
  buildInputs = [
    # pkgs.nodejs-8_x
    # pkgs.git
    purescript
    pkgs.nodePackages.bower
    pkgs.psc-package
  ];
  buildPhase = ''
    rm -rf node_modules output bower_components
    HOME=. npm install --verbose
    export SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt
    HOME=. ./bower update --force-latest
    npm run build
  '';
  checkPhase = ''
    npm run test
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp -r output/app.js node_modules $out/
    echo "#!${pkgs.bash}/bin/bash" > $out/bin/purescript-nix-bootstrap
    echo "${pkgs.nodejs-8_x}/bin/node $out/app.js" >> $out/bin/purescript-nix-bootstrap
    chmod +x $out/bin/purescript-nix-bootstrap
  '';
}
