import { Elm } from "./Main.elm";
import apple from "../Caterpillar/images/apple.png";

Elm.Css.Main.init({
  node: document.querySelector("main"),
  flags: { apple }
});
