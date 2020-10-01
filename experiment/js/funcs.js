const svgElements = [ "svg", "polygon", "circle", "rect", "path" ];
const borderWidth = "5px";
const mar = 5;
const len = 60;

/** Configurations */
const colorDict = {
  "very_dark": "#052e54",
  "dark": "#1155cc",
  "medium": "#6d9eeb",
  "light": "#c9daf8",
}
const allColors = Object.keys(colorDict);

function createCustomElement (type = 'div', className, id) {
  let element = (svgElements.indexOf(type) < 0)?
    document.createElement(type):
    document.createElementNS("http://www.w3.org/2000/svg", type);
  if (className.length > 0) element.setAttribute("class", className);
  element.setAttribute("id", id);
  return element;
}
function createDivWithStyle (className = "div", id = "", style = "") {
  let element = createCustomElement('div', className, id);
  setStyle(element, style);
  return element;
}
function createText(h = "h1", text = 'hello') {
  let element = document.createElement(h);
  let tx = document.createTextNode(text);
  element.append(tx);
  return(element)
}
function setAttributes(el, attrs) {
  for(var key in attrs) {
    el.setAttribute(key, attrs[key]);
  }
}

function createInputForm(taskId) {
  let form = createCustomElement("form", "input-form", `${taskId}-input-form`);
  const placeholderText = `Please type here`
  const options = `
    <option value="--" SELECTED>
    <option value="10">10 - Very certain</option>
    <option value="9">9</option>
    <option value="8">8</option>
    <option value="7">7</option>
    <option value="6">6</option>
    <option value="5">5 - Moderately</option>
    <option value="4">4</option>
    <option value="3">3</option>
    <option value="2">2</option>
    <option value="1">1</option>
    <option value="0">0 - Not sure at all</option>
  `
  form.innerHTML = `
    <p>
      <b>What is your ${taskId} impression about how these mysterious stones work?</b>
      (Please refer to stones as <i>active</i> and <i>inactive</i>,
      and be specific about <i>what properties you think matter or do not matter for the effects,
      and how they do so</i>.)
    </p>
    <textarea name="${taskId}_input" id="${taskId}-input" placeholder="${placeholderText}"></textarea>
    <p>How certain are you?
      <select name="${taskId}_certainty" id="${taskId}-certainty" class="input-rule">
        ${options}
      </select>
    </p>
    `
  return(form);
}
function disableFormInputs (formId) {
  const form = document.getElementById(formId);
  const inputs = form.elements;
  (Object.keys(inputs)).forEach((input) => inputs[input].disabled = true);
}
function showNext(id, display = "flex") {
  let div = document.getElementById(id);
  div.style.display = display;
  div.scrollIntoView(mode === 'dev'? false: true);
}
function hide(id) {
  let div = document.getElementById(id);
  div.style.display = "none";
}
function isFilled (formID) {
  let notFilled = false;
  const nulls = [ '', '--', '', '--', '', '--' ];
  const form = document.getElementById(formID);
  const inputs = form.elements;
  (Object.keys(inputs)).forEach((input, idx) => {
    let field = inputs[input];
    notFilled = (notFilled || (field.value === nulls[idx]));
  });
  return (!notFilled)
}
function playEffects (config) {
  const getCurrentLocation = (id) => {
    let rect = {top: 0, bottom: 0, left: 0, right: 0};
    const pos = document.getElementById(id).getBoundingClientRect();
    rect.top = pos.top;
    rect.bottom = pos.bottom;
    rect.left = pos.left;
    rect.right = pos.right;
    return rect;
  }
  if (!(document.body.contains(document.getElementById(`${config.taskId}-agent`)))) {
    createStones(config)
  }
  const agent = `${config.taskId}-agent`;
  const recipient = `${config.taskId}-recipient`;

  const agentStone = document.getElementById(agent);
  const startPos = getCurrentLocation(agent).right;
  const endPos = getCurrentLocation(recipient).left;

  const delta = Math.round(endPos - startPos) + 15;
  (delta > 0) && (agentStone.style.left = `${delta}px`);

  setTimeout(() => {
    let svg = document.getElementById(`${config.taskId}-recipient-svg`);
    clearElement(`${config.taskId}-recipient-stone`);
    svg = attachStone(svg, `${config.taskId}-result-stone`, getOpts(config.result, false));
  }, 1500);
}
function clearStones (config) {
  let els = [ "agent", "recipient" ].map(s => `${config.taskId}-${s}`);
  els.forEach (el => clearElement(el));
}
function clearElement (id) {
  let clear = document.getElementById(id);
  clear.remove();
}
function createSummaryStones(config, parentDiv, stoneClass = "new-stone") {
  createSumBox = (sumBox, type) => {
    let textDiv = createCustomElement("div", "sum-text", `${config.taskId}-sumbox-${type}-text`);
    textDiv.append(createText("h2", capFirstLetter(type)));
    let sumDiv = createCustomElement("div", "sum-display", `${config.taskId}-sumbox-${type}-display`);
    if (type === "before") {
      sumDiv.append(createStone(stoneClass, `${config.taskId}-agent`, getOpts(config.agent, true)));
      sumDiv.append(createStone(stoneClass, `${config.taskId}-recipient`, getOpts(config.recipient, false)));
      sumDiv.style.justifyContent = "space-between";
    } else if (type === "after") {
      sumDiv.append(createStone(stoneClass, `${config.taskId}-agent`, getOpts(config.agent, true)));
      sumDiv.append(createStone(stoneClass, `${config.taskId}-result`, getOpts(config.result, false)));
      sumDiv.style.justifyContent = "flex-end";
    } else {
      console.log("Summary type not match @createSummaryStones()")
    }
    sumBox.append(textDiv);
    sumBox.append(sumDiv);
    return sumBox;
  }
  let beforeBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-before`);
  let afterBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-after`);

  beforeBox = createSumBox(beforeBox, "before");
  afterBox = createSumBox(afterBox, "after");
  parentDiv.append(beforeBox);
  parentDiv.append(afterBox);
  return parentDiv;
}
function capFirstLetter (str) {
  let fl = str[0].toUpperCase();
  return(fl + str.slice(1,));
}
function currentSelection (formId) {
  let selections = [];
  const inputs = document.getElementById(formId).elements;
  (Object.keys(inputs)).forEach((input) => {
    selections.push(inputs[input].value)
  });
  return selections.reverse().join(";")
}
function saveFormData (input, dataObj) {
  let fieldName = input.name;
  dataObj[fieldName] = input.value;
  return dataObj;
}
function composeSelection (svgid, formid, checkBtnId) {
  const selections = currentSelection(formid).split(";");
  const confidence = selections[0];
  const color = selections[1];
  const shape = selections[2];
  const taskId = svgid.split('-').slice(0,2).join("-");

  if (!(color === "--" || shape === "--")) {
    let svg = document.getElementById(svgid);
    if (svg.childNodes.length > 0) {
      clearElement(`${taskId}-test-stone`)
    };
    svg = attachStone(svg, `${taskId}-test-stone`, getOpts(color+";"+shape));
  }
  let checkBtn = document.getElementById(checkBtnId);
  if (!(color === '--' || shape === '--' || confidence === '--')) checkBtn.disabled = false;
}
function createInitStones(config, parentDiv) {
  parentDiv.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, true)));
  parentDiv.append(createStone("new-stone", `${config.taskId}-recipient`, getOpts(config.recipient, false)));
  return(parentDiv);
}
function createStones (config) {
  let el = document.getElementById(`${config.taskId}-display-box`);
  el.append(createStone("new-stone", `${config.taskId}-agent`, getOpts(config.agent, true)));
  el.append(createStone("new-stone", `${config.taskId}-recipient`, getOpts(config.recipient, false)));
  return(el)
}
function createBtn (btnId, text = "Button", on = true, className = "task-button") {
  let btn = createCustomElement("button", className, btnId);
  btn.disabled = !on;
  (text.length > 0) ? btn.append(document.createTextNode(text)): null;
  return(btn)
}
function attachStone (svg, id, opts, shapeClass = 'shape') {
  if (Object.keys(opts).indexOf("points") < 0) {
    if (Object.keys(opts).indexOf("d") < 0) {
      svg.append(createCircle(shapeClass, `${id}`, opts));
    } else {
      svg.append(createDonut(shapeClass, `${id}`, opts));
    }
  } else {
    svg.append(createPolygon(shapeClass, `${id}`, opts))
  }
  return svg
}
function createStone (stoneClass, id, opts, svgClass = 'test', shapeClass = 'shape') {
  let div = createCustomElement("div", stoneClass, id);
  let svg = createCustomElement("svg", svgClass, `${id}-svg`);
  svg = attachStone(svg, `${id}-stone`, opts);
  div.append(svg)
  return(div);
}
function createPolygon(className, id, opts) {
  let polygon = createCustomElement("polygon", className, id);
  setAttributes(polygon, {
    "fill": opts.color,
    "points": opts.points,
    "stroke-width": opts.hasBorder? borderWidth : "0px",
  });
  return(polygon);
}
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array
}
function drawRdnNum(lower=1, upper=6, n=2) {
  const drawOne = (lower, upper) => Math.floor(Math.random() * (upper-lower+1)) + lower;
  let nums = [];
  nums.push(drawOne(lower, upper))
  if (n>1) {
    while(nums.length < n) {
      let m = drawOne(lower, upper);
      (nums.indexOf(m) < 0)? nums.push(m): null;
    }
  }
  return(nums)
}
function padNum (counter, n=2) {
  return(counter.toString().padStart(n, '0'))
}
