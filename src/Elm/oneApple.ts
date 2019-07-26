import { Elm } from "./OneApple.elm";
import apple from "../Caterpillar/images/apple.png";

Elm.Elm.OneApple.init({
  node: document.querySelector("main"),
  flags: { apple }
});
