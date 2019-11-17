{ stdenv, fetchFromGitHub, ocaml, findlib, benchmark }:

stdenv.mkDerivation rec {
  pname = "ocaml${ocaml.version}-pfds";
  version = "0.4";

  src = fetchFromGitHub {
    owner = "rixed";
    repo = "ocaml-pfds";
    rev = "v${version}";
    sha256 = "00ry328wmmnjdly7mw35sd9rdxw1lf93nwrr82dnfyp0bx1hcsbr";
  };

  buildInputs = [ ocaml findlib benchmark ];

  doCheck = true;

  createFindlibDestdir = true;

  meta = with stdenv.lib; {
    homepage = https://github.com/rixed/ocaml-pfds;
    description = "Purely Functional Data Structures, mostly from Okasaki";
    platforms = ocaml.meta.platforms or [];
    maintainers = [ maintainers.rixed ];
  };
}
