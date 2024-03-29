// Elm
import WebSlides from "webslides/src/js/modules/webslides";
import Slow from "../Caterpillar/SlowMain.elm";
import Fast from "../Caterpillar/FastMain.elm";
import { Elm as ElmOneApple } from "../Elm/OneApple.elm";
import { Elm as ElmTwoApples } from "../Elm/TwoApples.elm";

// CSS Animation
import { Elm as Css } from "../Css/Main.elm";

// Web Animation
import { Elm as JsOneApple } from "../Js/OneApple.elm";
import { Elm as JsTwoApples } from "../Js/TwoApples.elm";

// JavaScript
import hljs from "highlight.js";
import hljsElm from "highlight.js/lib/languages/elm";
import "highlight.js/styles/a11y-light.css";
import FpsEmitter from "fps-emitter";
import up from "up.js";

// Images
import apple from "../Caterpillar/images/apple.png";
import apples from "../Caterpillar/images/apples.png";
import caterpillarSmile from "../Caterpillar/images/caterpillar-smile.png";
import caterpillarFrown from "../Caterpillar/images/caterpillar-frown.png";
import sky from "../Caterpillar/images/sky.png";
import grass from "../Caterpillar/images/grass.png";
import fence from "../Caterpillar/images/fence.png";
import hillFar from "../Caterpillar/images/hill-far.png";
import hillNear from "../Caterpillar/images/hill-near.png";
import bush from "../Caterpillar/images/bush.png";
import cloud1 from "../Caterpillar/images/cloud-1.png";
import cloud2 from "../Caterpillar/images/cloud-2.png";
import sunSmile from "../Caterpillar/images/sun-smile.png";
import sunFrown from "../Caterpillar/images/sun-frown.png";
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

hljs.registerLanguage("elm", hljsElm);

const flags: any = {
  apple,
  apples,
  caterpillar: caterpillarSmile,
  caterpillarSmile,
  caterpillarFrown,
  sky,
  grass,
  fence,
  hillFar,
  hillNear,
  bush,
  cloud1,
  cloud2,
  sun: sunSmile,
  sunSmile,
  sunFrown,
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

Object.keys(flags).forEach((key: string) => {
  const value = flags[key];
  if (typeof value === "string") {
    up.link("prefetch", value);
  } else if (Array.isArray(value)) {
    value.forEach(grass => up.link("prefetch", grass));
  }
});

const ws: any = new WebSlides();
function isVisible(name: string): boolean {
  let node = document.getElementById(`${name}Container`);
  if (!node) return false;
  const section = node.closest("section");
  if (section) {
    return section.style.display !== "none";
  }
  return false;
}

let slowCaterpillar: any;
function startSlowCaterpillar(name: string, useStage: boolean) {
  let node = document.getElementById(name);
  if (node) {
    slowCaterpillar = Slow.Elm.Caterpillar.SlowMain.init({
      node,
      flags: { ...flags, useStage }
    });
  }
}

function startFastCaterpillar(name: string) {
  console.log("startFastCaterpillar");
  let node = document.getElementById(name);
  if (node) {
    Fast.Elm.Caterpillar.FastMain.init({
      node,
      flags
    });
  }
}

function startElmApp(
  name: string,
  elmApp: any = null,
  elmModule: any,
  startButton: HTMLElement | null = null
) {
  const actuallyStartTheApp = () => {
    console.log("actuallyStartTheApp");
    const node = document.getElementById(name);
    if (!elmApp && node && elmModule && isVisible(name)) {
      return elmModule.init({ node, flags: { apple } });
    }
  };

  if (startButton) {
    startButton.addEventListener("click", actuallyStartTheApp);
  } else {
    actuallyStartTheApp();
  }
}

function enableSyntaxHighlight() {
  document.querySelectorAll("code").forEach(block => {
    hljs.highlightBlock(block);
  });
}

function hideFpsCounter() {
  const fpsCounter = document.getElementById("fpsCounter");
  if (fpsCounter) {
    fpsCounter.style.display = "none";
  }
}

function showFpsCounter() {
  const fpsCounter = document.getElementById("fpsCounter");
  if (fpsCounter) {
    fpsCounter.style.display = "block";
  }
}

let elmOneApple: any = null;
let elmTwoApples: any = null;
let jsOneApple: any = null;
let jsTwoApples: any = null;
let cssApple: any = null;
function slideChangeHandler() {
  enableSyntaxHighlight();
  hideFpsCounter();

  let name = "slowCaterpillar";
  if (isVisible(name)) {
    startSlowCaterpillar(name, true);
    showFpsCounter();
  }

  name = "slowCaterpillarAgain";
  if (isVisible(name)) {
    startSlowCaterpillar(name, false);
    showFpsCounter();
  }

  name = "fastCaterpillar";
  if (isVisible(name)) {
    startFastCaterpillar(name);
    showFpsCounter();
  }

  name = "elmOneApple";
  if (isVisible(name)) {
    elmOneApple = startElmApp(
      name,
      elmOneApple,
      ElmOneApple.Elm.OneApple,
      document.getElementById("startElmOneAppleButton")
    );
  }

  name = "elmTwoApples";
  if (isVisible(name)) {
    elmOneApple = startElmApp(
      name,
      elmTwoApples,
      ElmTwoApples.Elm.TwoApples,
      document.getElementById("startElmTwoApplesButton")
    );
  }

  name = "jsOneApple";
  if (isVisible(name)) {
    jsOneApple = startElmApp(
      name,
      jsOneApple,
      JsOneApple.Js.OneApple,
      document.getElementById("startJsOneAppleButton")
    );
  }

  name = "jsTwoApples";
  if (isVisible(name)) {
    jsTwoApples = startElmApp(
      name,
      jsTwoApples,
      JsTwoApples.Js.TwoApples,
      document.getElementById("startJsTwoApplesButton")
    );
  }

  name = "cssApple";
  if (isVisible(name)) {
    cssApple = startElmApp(
      name,
      cssApple,
      Css.Css.Main,
      document.getElementById("startCssAppleButton")
    );
  }
}

function showLaserPointer(webslides: HTMLElement, event: MouseEvent) {
  webslides.className = "mousemoving";
}

function hideLaserPointer(webslides: HTMLElement) {
  window.requestAnimationFrame(() => {
    webslides.className = "mousestop";
  });
}

function initializeLaserPointer() {
  const webslides = document.getElementById("webslides");
  let mouseTimer: number;

  if (webslides) {
    webslides.addEventListener("mousemove", (event: MouseEvent) => {
      if (mouseTimer) {
        clearTimeout(mouseTimer);
      }

      mouseTimer = setTimeout(() => {
        hideLaserPointer(webslides);
      }, 1000);

      showLaserPointer(webslides, event);
    });
  }
}

ws.el.addEventListener("ws:slide-change", slideChangeHandler);

document.addEventListener("DOMContentLoaded", () => {
  initializeLaserPointer();
  const fpsCounter = document.getElementById("fpsCounter");
  if (fpsCounter) {
    const fps = new FpsEmitter();
    fps.on("update", (currentFps: number) => {
      fpsCounter.textContent = "fps: " + currentFps;
      if (slowCaterpillar) {
        slowCaterpillar.ports.fps.send(currentFps);
      }
    });
  }
  slideChangeHandler();
});
