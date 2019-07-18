class ElmAnimation extends HTMLElement {
  constructor(private currentAnimation: Animation | null) {
    super();
  }

  static get observedAttributes() {
    return ["animate"];
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    console.log("attributeChangedCallback", name);
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

    this.currentAnimation = this.children[0].animate(
      animationData.keyframes,
      animationData.options
    );

    this.currentAnimation.onfinish = () => {
      console.log("finish");
      this.dispatchEvent(new Event("finish"));
    };

    return this.currentAnimation;
  }

  connectedCallback() {
    const animateJson = this.getAttribute("animate");
    if (animateJson) {
      this.animateContent(animateJson);
    }
  }
}

document.addEventListener("DOMContentLoaded", function(event) {
  customElements.define("elm-animation", ElmAnimation);
});
