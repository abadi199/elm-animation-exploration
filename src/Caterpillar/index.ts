import { Elm } from "./Main.elm";
import apple from "./images/apple.png";

console.log(apple);

Elm.Caterpillar.Main.init({
  node: document.querySelector("main"),
  flags: { apple }
});
