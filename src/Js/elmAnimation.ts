class ElmAnimation extends HTMLElement {
  constructor(private currentAnimation: Animation | null) {
    super();
  }

  static get observedAttributes() {
    return ["animate", "playback"];
  }

  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    switch (name) {
      case "animate":
        this.animateContent(newValue);
        break;
      case "playback":
        this.playback(newValue);
        break;
    }
  }

  playback(value: string): void {
    if (!this.currentAnimation) {
      return;
    }

    switch (value) {
      case "play":
        this.currentAnimation.play();
        break;
      case "pause":
        this.currentAnimation.pause();
        break;
    }
  }

  animateContent(json: string): Animation | null {
    const animationData: any = JSON.parse(json);

    if (!this.children || this.children.length === 0) {
      return null;
    }

    if (this.currentAnimation) {
      this.currentAnimation.cancel();
    }

    if (animationData.keyframes.length === 0) {
      return null;
    }

    this.currentAnimation = this.children[0].animate(
      animationData.keyframes,
      animationData.options
    );

    this.currentAnimation.onfinish = () => {
      this.dispatchEvent(new Event("finish"));
    };

    return this.currentAnimation;
  }

  connectedCallback() {
    const animateJson = this.getAttribute("animate");

    if (animateJson) {
      this.animateContent(animateJson);
    }

    const playbackValue = this.getAttribute("playback");
    if (playbackValue) {
      this.playback(playbackValue);
    }
  }
}

document.addEventListener("DOMContentLoaded", function(event) {
  customElements.define("elm-animation", ElmAnimation);
});
