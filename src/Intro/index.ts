// Elm
import { Elm } from "./Tea.elm";

import WebSlides from "webslides/src/js/modules/webslides";

// JavaScript
import hljs from "highlight.js";
import hljsElm from "highlight.js/lib/languages/elm";
import "highlight.js/styles/a11y-light.css";

hljs.registerLanguage("elm", hljsElm);

const ws: any = new WebSlides();

function enableSyntaxHighlight() {
  document.querySelectorAll("code").forEach(block => {
    hljs.highlightBlock(block);
  });
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

function startElmTea() {
  const node = document.getElementById("elmTea");
  if (node) {
    Elm.Intro.Tea.init({ node });
  }
}

document.addEventListener("DOMContentLoaded", () => {
  initializeLaserPointer();
  enableSyntaxHighlight();
  startElmTea();
});
