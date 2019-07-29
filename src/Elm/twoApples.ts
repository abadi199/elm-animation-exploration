import { Elm } from "./TwoApples.elm";
import apple from "../Caterpillar/images/apple.png";

Elm.Elm.TwoApples.init({
  node: document.querySelector("main"),
  flags: { apple }
});
