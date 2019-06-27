import { Elm } from "./Main.elm";
import apple from "./images/apple.png";
import caterpillar from "./images/caterpillar.png";

Elm.Caterpillar.Main.init({
  node: document.querySelector("main"),
  flags: { apple, caterpillar }
});
