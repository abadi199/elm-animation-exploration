import { Elm } from "./Main.elm";
import apple from "./images/apple.png";
import apples from "./images/apples.png";
import caterpillar from "./images/caterpillar.png";
import sky from "./images/sky.png";
import grass from "./images/grass.png";
import fence from "./images/fence.png";
import hillFar from "./images/hill-far.png";
import hillNear from "./images/hill-near.png";
import bush from "./images/bush.png"
import cloud1 from "./images/cloud-1.png";
import cloud2 from "./images/cloud-2.png";
import sun from "./images/sun.png";
import tree from "./images/tree.png";

Elm.Caterpillar.Main.init({
  node: document.querySelector("main"),
  flags: {
    apple, apples, caterpillar, sky, grass, fence, hillFar,
    hillNear, bush, cloud1, cloud2, sun, tree
  }
});
