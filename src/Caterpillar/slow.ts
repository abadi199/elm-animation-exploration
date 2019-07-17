import { Elm } from "./SlowMain.elm";
import apple from "./images/apple.png";
import apples from "./images/apples.png";
import caterpillar from "./images/caterpillar.png";
import sky from "./images/sky.png";
import grass from "./images/grass.png";
import fence from "./images/fence.png";
import hillFar from "./images/hill-far.png";
import hillNear from "./images/hill-near.png";
import bush from "./images/bush.png";
import cloud1 from "./images/cloud-1.png";
import cloud2 from "./images/cloud-2.png";
import sun from "./images/sun.png";
import sunRays from "./images/sun-rays.png";
import tree from "./images/tree.png";
import grassAll from "./images/grass-all.png";
import grass01 from "./images/grass-01.png";
import grass02 from "./images/grass-02.png";
import grass03 from "./images/grass-03.png";
import grass04 from "./images/grass-04.png";
import grass05 from "./images/grass-05.png";
import grass06 from "./images/grass-06.png";
import grass07 from "./images/grass-07.png";
import grass08 from "./images/grass-08.png";
import grass09 from "./images/grass-09.png";
import grass10 from "./images/grass-10.png";
import grass11 from "./images/grass-11.png";
import grass12 from "./images/grass-12.png";
import grass13 from "./images/grass-13.png";
import grass14 from "./images/grass-14.png";

Elm.Caterpillar.SlowMain.init({
  node: document.querySelector("main"),
  flags: {
    apple,
    apples,
    caterpillar,
    sky,
    grass,
    fence,
    hillFar,
    hillNear,
    bush,
    cloud1,
    cloud2,
    sun,
    sunRays,
    tree,
    grassAll,
    grasses: [
      grass01,
      grass02,
      grass03,
      grass04,
      grass05,
      grass06,
      grass07,
      grass08,
      grass09,
      grass10,
      grass11,
      grass12,
      grass13,
      grass14
    ]
  }
});
