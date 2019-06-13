class ElmAnimation extends HTMLElement {
  constructor() {
    super();
  }

  static get observedAttributes() {
    return [];
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) { }

  connectedCallback() {
    const animation = this.children[0].animate(
      [
        { transform: "rotate(0)" },
        {
          transform: "rotate(360deg)"
        }
      ],
      {
        duration: 3000,
        iterations: Infinity
      }
    );

    animation.onfinish = (ev: AnimationPlaybackEvent) => {
      this.dispatchEvent(new Event("finish"));
    };
  }
}

document.addEventListener("DOMContentLoaded", function(event) {
  customElements.define("elm-animation", ElmAnimation);
});
