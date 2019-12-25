let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cases"; version = "0.1.3.2"; };
      license = "MIT";
      copyright = "(c) 2014, Nikita Volkov";
      maintainer = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      author = "Nikita Volkov <nikita.y.volkov@mail.ru>";
      homepage = "https://github.com/nikita-volkov/cases";
      url = "";
      synopsis = "A converter for spinal, snake and camel cases";
      description = "A parser-based converter library for spinal, snake and camel cases.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."attoparsec" or (buildDepError "attoparsec"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."loch-th" or (buildDepError "loch-th"))
          (hsPkgs."base-prelude" or (buildDepError "base-prelude"))
          ];
        buildable = true;
        };
      tests = {
        "api-tests" = {
          depends = [
            (hsPkgs."cases" or (buildDepError "cases"))
            (hsPkgs."QuickCheck" or (buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (buildDepError "HUnit"))
            (hsPkgs."HTF" or (buildDepError "HTF"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."loch-th" or (buildDepError "loch-th"))
            (hsPkgs."placeholders" or (buildDepError "placeholders"))
            (hsPkgs."base" or (buildDepError "base"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "benchmark" = {
          depends = [
            (hsPkgs."cases" or (buildDepError "cases"))
            (hsPkgs."mwc-random" or (buildDepError "mwc-random"))
            (hsPkgs."criterion" or (buildDepError "criterion"))
            (hsPkgs."rerebase" or (buildDepError "rerebase"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/srghma/cases.git";
      rev = "a579b01d589f8e533d2978fa4f9ded6a83e281b4";
      sha256 = "11iz34x3zfc2816z8hxy5y7ckvrq47mi4y1bnchflwiymjix747v";
      });
    }