const svgElements = [ "svg", "polygon", "circle", "rect", "path" ];
const defaultStone = { 'borderWidth': '8px', 'mar': 5, 'len': 60 };
const smallStone = { 'borderWidth': '3px', 'mar': 3, 'len': 20 };

/** Configurations */
const colorDict = {
  "light": "#c9daf8",
  "medium": "#6d9eeb",
  "dark": "#1155cc",
  "very_dark": "#052e54",
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
function createTextInputPanel (taskId) {
  let taskBox = createCustomElement("div", "input-div", `${taskId}-input`);

  const displayBox = createCustomElement("div", "input-box", `${taskId}-input-box`);
  displayBox.append(createInputForm(taskId));

  const buttonGroup = createCustomElement("div", "button-group-vc", `${taskId}-button-group`);
  buttonGroup.append(createBtn(`${taskId}-input-submit-btn`, "OK", false));
  // buttonGroup.append(createBtn(`${taskId}-input-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);
  return taskBox;
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
  div.scrollIntoView(true);
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
function createSumBox(sumBox, type, config, stoneClass) {
  let textDiv = createText("h2", capFirstLetter(type));
    let sumDiv = createCustomElement("div", "sum-display", `${config.taskId}-sumbox-${type}-display`);
    if (type === "before") {
      sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-before-agent`, getOpts(config.agent, true, 's'), 'sum'));
      sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-before-recipient`, getOpts(config.recipient, false, 's'), 'sum'));
      sumDiv.style.justifyContent = "space-between";
    } else if (type === "after") {
      sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-agent`, getOpts(config.agent, true, 's'), 'sum'));
      sumDiv.append(createStone(stoneClass, `${config.taskId}-sum-after-result`, getOpts(config.result, false, 's'), 'sum'));
      sumDiv.style.justifyContent = "flex-end";
    } else {
      console.log("Summary type not match @createSummaryStones()")
    }
    sumBox.append(textDiv);
    sumBox.append(sumDiv);
    return sumBox;
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
function composeSelection (svgid, formid, checkBtnId) {
  const selections = currentSelection(formid).split(";");
  const confidence = selections[0];
  const color = selections[1];
  const shape = selections[2];
  const taskId = svgid.split('-').slice(0,2).join("-");

  if (!(color === "--" || shape === "--")) {
    let stoneCode = parseInt(shape.slice(1,) + color.slice(1,))
    let svg = document.getElementById(svgid);
    if (svg.childNodes.length > 0) {
      clearElement(`${taskId}-test-stone`)
    };
    svg = attachStone(svg, `${taskId}-test-stone`, getOpts(stoneCode));
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
function createStone (stoneClass, id, opts, svgClass = 'test') {
  let div = createCustomElement("div", stoneClass, id);
  let svg = createCustomElement("svg", svgClass, `${id}-svg`);
  svg = attachStone(svg, `${id}-stone`, opts);
  div.append(svg)
  return(div);
}
function createPolygon(className, id, opts) {
  let polygon = createCustomElement("polygon", className, id);
  let borderWidth = (opts.scale==='default')? defaultStone.borderWidth: smallStone.borderWidth;
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
function getOpts(int, isAgent, scale='default') {
  const edges = Math.floor(int / 10);
  const shading = int % 10;

  let opts = {};
  opts["color"] = colorDict[Object.keys(colorDict)[shading-1]]
  opts["hasBorder"] = isAgent;
  opts["points"] = calcPolygon(edges, scale);
  opts["scale"] = scale;
  return opts
}

function calcPolygon(n, scale) {
  n = parseInt(n);
  let output = [];
  let adjust = (n===5)? 55 : 0;

  const mar = (scale==='default')? defaultStone.mar: smallStone.mar;
  const len = (scale==='default')? defaultStone.len: smallStone.len;

  if (n === 3) {
    output.push(`${len/2},${mar}`);
    output.push(`${len-mar},${len-mar}`);
    output.push(`${mar},${len-mar}`);
  } else if (n === 4) {
    output.push(`${mar},${mar}`);
    output.push(`${len-mar},${mar}`);
    output.push(`${len-mar},${len-mar}`);
    output.push(`${mar},${len-mar}`);
  } else {
    // Adapted from https://gist.github.com/jonthesquirrel/e2807811d58a6627ded4
    for (let i = 1; i <= n; i++) {
      output.push(
        ((len/2 * Math.cos(adjust + 2 * i * Math.PI / n)) + len/2).toFixed(0).toString() + "," +
        ((len/2 * Math.sin(adjust + 2 * i * Math.PI / n)) + len/2).toFixed(0).toString()
      )
    }
  }
  return output.join(" ")
}
function createAnswerComposer(config) {
  const taskId = config.taskId;
  let box = createCustomElement("div", "display-box", `${taskId}-selection-box`);
  box.style.width = "48%";

  box.innerHTML = `
    <div class="selection-composer">
      <div class="selection-form-div">
        <form class="selection-form" id="${taskId}-selection-form">
          <p>Shape:
          <select id="shape" name="shape" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="p3">Triangle</option>
            <option value="p4">Square</option>
            <option value="p5">Pentagon</option>
            <option value="p6">Hexagon</option>
            <option value="p7">Heptagon</option>
          </select>
          </p>
          <p>Shading:
          <select id="color" name="color" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="s1">Light</option>
            <option value="s2">Medium</option>
            <option value="s3">Dark</option>
            <option value="s4">Very dark</option>
          </select>
          <p>Your confidence:
          <select id="conf" name="conf" class="selection-input">
            <option value="--" SELECTED>--</option>
            <option value="10">10 - Very confident</option>
            <option value="9">9</option>
            <option value="8">8</option>
            <option value="7">7</option>
            <option value="6">6</option>
            <option value="5">5 - Moderately confident</option>
            <option value="4">4</option>
            <option value="3">3</option>
            <option value="2">2</option>
            <option value="1">1 - Not confident at all</option>
          </select>
          </p>
        </form>
      </div>
      <div class="selection-svg-div">
        <svg class="selection-object" id='${taskId}-selection-svg'></svg>
      </div>
      <div class="selection-buttons">
        <button class="task-button" id="${taskId}-confirm-btn" disabled>OK</button>
      </div>
    </div>`
  return box;
}
function showPostCheckPage (isPass) {
  const pageDiv = isPass? 'pass' : 'retry';
  document.getElementById('check-btn').style.display = 'none';
  document.getElementById(pageDiv).style.display = 'flex';
}
function compIsFilled () {
  let radios = document.getElementsByTagName('input');
  let checked = 0;
  for (let i = 0; i < radios.length; i++) {
      checked += radios[i].checked;
  }
  return (checked > checks.length-1)
}
function showCompletion(code) {
  hide("debrief")
  showNext("completed")
  let t = document.createTextNode(code);
  document.getElementById('completion-code').append(t);
}
function generateToken (length) {
  let tokens = '';
  let chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  for (let i = 0; i < length; i ++) {
      tokens += chars.charAt(Math.floor(Math.random() * chars.length))
  }
  return tokens;
}
function formatDates (date, option = 'date') {
  let year = date.getFullYear();
  let month = String(date.getMonth() + 1).padStart(2, '0');
  let day = String(date.getDate() + 1).padStart(2, '0');
  let hour = String(date.getHours()+ 1).padStart(2, '0');
  let min = String(date.getMinutes() + 1).padStart(2, '0');
  let sec = String(date.getSeconds() + 1).padStart(2, '0');
  dateParts = (option === 'date') ? [ year, month, day ] : [ hour, min, sec ];
  return dateParts.join('_');
}
function download(content, fileName, contentType) {
  var a = document.createElement("a");
  var file = new Blob([content], {type: contentType});
  a.href = URL.createObjectURL(file);
  a.download = fileName;
  a.click();
}
