import Prism from "prismjs";
import "prismjs/components/prism-elm";
import "prismjs/components/prism-json";
import "prismjs/components/prism-properties";
import "prismjs/themes/prism-tomorrow.css";

function getCursorPosition(parent, node, offset, stat) {
  if (stat.done) return stat;

  let currentNode = null;
  if (parent.childNodes.length == 0) {
    stat.pos += parent.textContent.length;
  } else {
    for (let i = 0; i < parent.childNodes.length && !stat.done; i++) {
      currentNode = parent.childNodes[i];
      if (currentNode === node) {
        stat.pos += offset;
        stat.done = true;
        return stat;
      } else getCursorPosition(currentNode, node, offset, stat);
    }
  }
  return stat;
}

//find the child node and relative position and set it on range
function setCursorPosition(parent, range, stat) {
  if (stat.done) return range;

  if (parent.childNodes.length == 0) {
    if (parent.textContent.length >= stat.pos) {
      range.setStart(parent, stat.pos);
      stat.done = true;
    } else {
      stat.pos = stat.pos - parent.textContent.length;
    }
  } else {
    for (let i = 0; i < parent.childNodes.length && !stat.done; i++) {
      const currentNode = parent.childNodes[i];
      setCursorPosition(currentNode, range, stat);
    }
  }
  return range;
}

export class CodeComponent extends HTMLElement {
  codeEl;
  restore;

  static get observedAttributes() {
    return ["code", "lang", "pos", "editable"];
  }

  constructor() {
    super();
    this.onEdit = this.onEdit.bind(this);
  }

  onEdit() {
    const sel = window.getSelection();
    const node = sel.focusNode;
    const offset = sel.focusOffset;
    const { pos } = getCursorPosition(this.codeEl, node, offset, {
      pos: 0,
      done: false,
    });

    this.dispatchEvent(
      new CustomEvent("edit", {
        detail: { content: this.codeEl.textContent, caretPos: pos },
      })
    );
  }

  connectedCallback() {
    const pre = document.createElement("pre");
    this.codeEl = document.createElement("code");

    this.codeEl.addEventListener("input", this.onEdit);

    const lang = this.getAttribute("lang");
    const code = this.getAttribute("code");
    const pos = this.getAttribute("pos");
    const editable = this.getAttribute("editable");
    lang && this.setLang(lang);
    code && this.setCode(code);
    pos && this.setCaretPosition(pos);
    editable && this.codeEl.setAttribute("contentEditable", !!editable);

    pre.appendChild(this.codeEl);
    this.appendChild(pre);

    Prism.highlightElement(this.codeEl);
  }

  disconnectedCallback() {
    this.codeEl.removeEventListener("input", this.onEdit);
  }

  attributeChangedCallback(attrName, _oldVal, newVal) {
    if (attrName === "lang") {
      this.setLang(newVal);
    }
    if (attrName === "code") {
      this.setCode(newVal);
    }
    if (attrName === "editable") {
      console.log("Bla", newVal);
      this.codeEl && this.codeEl.setAttribute("contentEditable", !!newVal);
    }
    if (this.codeEl) {
      Prism.highlightElement(this.codeEl);
    }

    if (attrName === "pos") {
      this.setCaretPosition(newVal);
    }
  }

  setLang(lang) {
    if (this.codeEl) {
      this.codeEl.setAttribute("class", `language-${lang}`);
    }
  }

  setCode(code) {
    if (this.codeEl) {
      this.codeEl.innerHTML = code;
    }
  }

  setCaretPosition(pos) {
    if (this.codeEl) {
      const sel = window.getSelection();
      sel.removeAllRanges();
      const range = setCursorPosition(this.codeEl, document.createRange(), {
        pos,
        done: false,
      });
      range.collapse(true);
      sel.addRange(range);
    }
  }
}

export const registerCodeComponent = () => {
  window.customElements.define("highlighted-code", CodeComponent);
};
