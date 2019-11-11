class Animator extends HTMLElement {
  constructor() {
    super();
  }

  static get observedAttributes() {
    return ["transitioning"];
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    switch (name) {
      case "transitioning":
        requestAnimationFrame(() => {
          this.transitioning();
        });
        break;

      default:
        break;
    }
  }

  transitioning() {
    const kind = this.getAttribute("kind");
    const fromElements = this.getElementsByTagName("elm-animator-from");
    const toElements = this.getElementsByTagName("elm-animator-to");

    if (fromElements.length === 0 || toElements.length === 0) {
      return;
    }

    const from = fromElements[0] as HTMLElement;
    const to = toElements[0] as HTMLElement;

    from.style.display = "inline-block";
    to.style.display = "inline-block";

    switch (kind) {
      case "SlideInFromTop":
        this.slideInFromTop(from, to);
        break;

      case "Fade":
        this.fade(from, to);
        break;

      default:
        break;
    }
  }

  slideInFromTop(from: HTMLElement, to: HTMLElement) {
    const duration = 500;
    const options: KeyframeAnimationOptions = {
      duration,
      iterations: 1,
      fill: "forwards",
      easing: "ease-in-out"
    };

    from.animate(
      [{ transform: "translateY(-100%)" }, { transform: "translateY(0%)" }],
      options
    );

    to.animate(
      [{ transform: "translateY(-100%)" }, { transform: "translateY(0%)" }],
      options
    ).addEventListener("finish", () => {
      this.dispatchEvent(new Event("finish"));
    });
  }

  fade(from: HTMLElement, to: HTMLElement) {
    console.log("fade");

    const duration = 500;
    const options: KeyframeAnimationOptions = {
      duration,
      iterations: 1,
      fill: "forwards",
      easing: "ease-in-out"
    };

    from.animate([{ opacity: "1" }, { opacity: "0" }], options);

    to.animate([{ opacity: "0" }, { opacity: "1" }], options).addEventListener(
      "finish",
      () => {
        this.dispatchEvent(new Event("finish"));
      }
    );
  }

  connectedCallback() {
    const transitioning = this.getAttribute("transitioning");
    if (transitioning) {
      this.transitioning();
    }
  }
}

document.addEventListener("DOMContentLoaded", function(event) {
  customElements.define("elm-animator", Animator);
});
