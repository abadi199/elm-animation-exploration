class ElmAnimation extends HTMLElement {
  constructor(private currentAnimation: Animation | null) {
    super();
  }

  static get observedAttributes() {
    return ["animate"];
  }

  attributeChangedCallack(name: string, oldValue: string, newValue: string) {
    console.log(name);
    switch (name) {
      case "animate":
        this.animateContent(newValue);
    }
  }

  animateContent(json: string): Animation | null {
    const animationData: any = JSON.parse(json);

    if (!this.children || this.children.length === 0) {
      return null;
    }

    if (this.currentAnimation) {
      this.currentAnimation.finish();
    }

    return this.children[0].animate(
      animationData.keyframes,
      animationData.options
    );
  }

  connectedCallback() {
    const animateJson = this.getAttribute("animate");
    if (animateJson) {
      this.currentAnimation = this.animateContent(animateJson);
    }
  }
}

document.addEventListener("DOMContentLoaded", function(event) {
  customElements.define("elm-animation", ElmAnimation);
});
