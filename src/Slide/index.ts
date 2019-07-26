// Elm
import WebSlides from "webslides/src/js/modules/webslides";
import Slow from "../Caterpillar/SlowMain.elm";
import Fast from "../Caterpillar/FastMain.elm";
import { Elm as ElmOneApple } from "../Elm/OneApple.elm";

// JavaScript
import hljs from "highlight.js";
import hljsElm from "highlight.js/lib/languages/elm";
import "highlight.js/styles/a11y-light.css";

// Images
import apple from "../Caterpillar/images/apple.png";
import apples from "../Caterpillar/images/apples.png";
import caterpillar from "../Caterpillar/images/caterpillar.png";
import sky from "../Caterpillar/images/sky.png";
import grass from "../Caterpillar/images/grass.png";
import fence from "../Caterpillar/images/fence.png";
import hillFar from "../Caterpillar/images/hill-far.png";
import hillNear from "../Caterpillar/images/hill-near.png";
import bush from "../Caterpillar/images/bush.png";
import cloud1 from "../Caterpillar/images/cloud-1.png";
import cloud2 from "../Caterpillar/images/cloud-2.png";
import sun from "../Caterpillar/images/sun.png";
import sunRays from "../Caterpillar/images/sun-rays.png";
import tree from "../Caterpillar/images/tree.png";
import grassAll from "../Caterpillar/images/grass-all.png";
import grass01 from "../Caterpillar/images/grass-01.png";
import grass02 from "../Caterpillar/images/grass-02.png";
import grass03 from "../Caterpillar/images/grass-03.png";
import grass04 from "../Caterpillar/images/grass-04.png";
import grass05 from "../Caterpillar/images/grass-05.png";
import grass06 from "../Caterpillar/images/grass-06.png";
import grass07 from "../Caterpillar/images/grass-07.png";
import grass08 from "../Caterpillar/images/grass-08.png";
import grass09 from "../Caterpillar/images/grass-09.png";
import grass10 from "../Caterpillar/images/grass-10.png";
import grass11 from "../Caterpillar/images/grass-11.png";
import grass12 from "../Caterpillar/images/grass-12.png";
import grass13 from "../Caterpillar/images/grass-13.png";
import grass14 from "../Caterpillar/images/grass-14.png";

const SLOW_CATERPILLAR_INDEX = 2;
const FAST_CATERPILLAR_INDEX = 27;
const ELM_APPLE_ONE = 9;

hljs.registerLanguage("elm", hljsElm);

const flags = {
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
};

const ws: any = new WebSlides();
ws.el.addEventListener("ws:init", console.log);

let slowCaterpillar: any = null;
function startSlowCaterpillar(node) {
  if (slowCaterpillar) {
    slowCaterpillar.ports.pause.send(false);
  } else {
    slowCaterpillar = Slow.Elm.Caterpillar.SlowMain.init({
      node,
      flags
    });
  }
}

let fastCaterpillar: any = null;
function startFastCaterpillar(node: HTMLElement) {
  if (fastCaterpillar) {
    fastCaterpillar.ports.pause.send(false);
  } else {
    fastCaterpillar = Fast.Elm.Caterpillar.FastMain.init({
      node,
      flags
    });
  }
}

let elmOneApple: any = null;
function startElmOneApple(node: HTMLElement) {
  if (elmOneApple) {
  } else {
    elmOneApple = ElmOneApple.Elm.OneApple.init({ node, flags: { apple } });
  }
}

function pauseAll() {
  if (slowCaterpillar) {
    slowCaterpillar.ports.pause.send(true);
  }
  if (fastCaterpillar) {
    fastCaterpillar.ports.pause.send(true);
  }
}

function enableSyntaxHighlight() {
  document.querySelectorAll("code").forEach(block => {
    hljs.highlightBlock(block);
  });
}

function runElmApps() {
  enableSyntaxHighlight();

  let node = document.getElementById("slowCaterpillar");
  if (node) {
    startSlowCaterpillar(node);
  }

  node = document.getElementById("fastCaterpillar");
  if (node) {
    startFastCaterpillar(node);
  }

  node = document.getElementById("elmOneApple");
  if (node) {
    startElmOneApple(node);
  }

  if (!node) {
    pauseAll();
  }
}

ws.el.addEventListener("ws:slide-change", runElmApps);
runElmApps();
