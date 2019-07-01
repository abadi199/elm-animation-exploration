import { Elm } from "./Main.elm";
import apple from "./images/apple.png";
import apples from "./images/apples.png";
import caterpillar from "./images/caterpillar.png";
import sky from "./images/sky.png";
import grass from "./images/grass.png";

Elm.Caterpillar.Main.init({
  node: document.querySelector("main"),
  flags: { apple, caterpillar, sky, grass }
});
