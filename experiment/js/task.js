
const mode = 'dev' // '' for production, 'dev' for development, 'flask' for flask-app

/** Pick a condition */
const conditions = [ 'A1', 'A2', 'A3', 'A4' ];
const cond = conditions[drawRdnNum(0,3,1)]
config = config.filter(c => c.group === cond);

// const cond = config[0].group
console.log(`${mode} mode; condition ${cond}.`);

/** Comprehension quiz */
const start_time = Date.now();
let start_task_time = 0;

document.getElementById('desc-next-btn').onclick = () => {
  hide("box-desc-1")
  showNext("box-desc-2", "block")
}

document.getElementById('desc-btn').onclick = () => {
  hide("box-desc-2");
  showNext("box-desc-1", "block");
  hide('instruction');
  showNext("comprehension", "block");
  showNext("check-btn");
}

const checkBtn = document.getElementById('check-btn');
const checks = [ 'check1', 'check2', 'check3', 'check4', 'check5' ];
const answers = [ false, false, true, true, true ];

const passBtn = document.getElementById('pass-btn');
const retryBtn = document.getElementById('retry-btn');

checkBtn.onclick = () => {
  let inputs = [];
  checks.map(check => {
    const vals = document.getElementsByName(check);
    inputs.push(vals[0].checked);
  });
  const pass = (inputs.join('') === answers.join(''));
  showPostCheckPage(pass);
}

passBtn.onclick = () => {
  start_task_time = Date.now();
  hide("pass");
  hide("comprehension");
  showNext("tasks", "block");
};
retryBtn.onclick = () => {
  hide("retry");
  hide("comprehension");
  showNext("instruction", "block")
};

document.getElementById('prequiz').onchange = () => compIsFilled() ? checkBtn.disabled = false : null;


/** Prep data */
let subjectData = {};

let learnSids = [];
let genSigs = [];
config.forEach(c => (c.phase==='learn')? learnSids.push(c.sid): genSigs.push(c.sid));

// Shuffle them
learnSids = shuffleArray(learnSids);
// Add two learning tasks into gen set for verification
(drawRdnNum(0, learnSids.length-1, 2)).forEach(i => genSigs.push(learnSids[i]));
genSigs = shuffleArray(genSigs);

let learnConfigs = []
learnSids.forEach((sid, idx) => {
  let cfg = (config.filter(c => c.sid === sid))[0];
  let taskId = 'learn-' + padNum(idx+1);
  learnConfigs.push([taskId, sid, cfg.agent, cfg.recipient, cfg.result])
})

let genConfigs = []
genSigs.forEach((sid, idx) => {
  let cfg = (config.filter(c => c.sid === sid))[0];
  let taskId = 'gen-' + padNum(idx+1);
  genConfigs.push([taskId, sid, cfg.agent, cfg.recipient, cfg.result])
})

let trialData = {
  "phase": [],
  "tid": [],
  "sid": [],
  "agent": [],
  "recipient": [],
  "result": [],
};
learnConfigs.forEach(c => {
  trialData['phase'].push('learn');
  trialData['tid'].push(c[0]);
  trialData['sid'].push(c[1]);
  trialData['agent'].push(c[2]);
  trialData['recipient'].push(c[3]);
  trialData['result'].push(c[4]);
})
genConfigs.forEach(c => {
  trialData['phase'].push('gen');
  trialData['tid'].push(c[0]);
  trialData['sid'].push(c[1]);
  trialData['agent'].push(c[2]);
  trialData['recipient'].push(c[3]);
})

let showDiv = document.getElementById("showcase");
const coreLearnDiv = document.getElementById("core-learn-div");
let learnClicked = Array(learnConfigs.length).fill(0);

for(let i = 0; i < learnConfigs.length; i++ ) {
  const taskConfig = learnConfigs[i];
  const config = {
    'taskId': taskConfig[0],
    'agent': taskConfig[2],
    'recipient': taskConfig[3],
    'result': taskConfig[4]
  };

  /** Showcase contents */
  let showBox = document.getElementById(`showcase-${i+1}`);
  let boxWrapper = createCustomElement("div", "sum-wrap", `${config.taskId}-sumwrap`);

  let beforeBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-before`);
  let afterBox = createCustomElement("div", "sum-box", `${config.taskId}-sumbox-after`);
  beforeBox = createSumBox(beforeBox, "before", config, 'sum');
  afterBox = createSumBox(afterBox, "after", config, 'sum');

  boxWrapper.append(beforeBox);
  boxWrapper.append(afterBox);
  showBox.append(boxWrapper);
  boxWrapper.style.display= (mode==='dev')? 'flex': 'none';

  /** Core: learn tasks */
  const taskId = config.taskId;
  let display = (mode==='dev'|i===0)? 'flex': 'none';

  let box = createCustomElement("div", "box", `box-${taskId}`);
  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);

  let taskNum = createText('h2', i+1 + '/' + learnConfigs.length);
  taskBox.append(taskNum);

  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  displayBox = createInitStones(config, displayBox);

  const buttonGroup = createCustomElement("div", "button-group-vc", `${taskId}-button-group`);
  buttonGroup.append(createBtn(`${taskId}-play-btn`, "Test", true));
  buttonGroup.append(createBtn(`${taskId}-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);
  box.append(taskBox);
  box.style.display = display;
  coreLearnDiv.append(box);

  /** Button functionalities */
  const playBtn = document.getElementById(`${taskId}-play-btn`);
  const nextBtn = document.getElementById(`${taskId}-next-btn`);

  playBtn.onclick = () => {
    playBtn.disabled = true;
    if (learnClicked[i] > 0) {
      clearStones(config);
      createInitStones(config, displayBox)
    }
    playEffects(config);
    setTimeout(() => {
      nextBtn.disabled = false;
      playBtn.disabled = false;
      playBtn.innerText = 'Test again'
      boxWrapper.style.display = 'flex';
    }, 2000);
    learnClicked[i] += 1;
  }
  nextBtn.onclick = () => {
    nextBtn.disabled = true;
    const nextDiv = (i === learnConfigs.length-1)? "core-learn-form-div": `box-learn-${padNum(i+2)}`;
    (mode !== 'dev')? hide(`box-${taskId}`): null;
    showNext(nextDiv, 'flex');
  }
}
// coreLearnDiv.style.display = 'none';

/** Core: initial input form */
let initialInput = document.getElementById("core-learn-form-div");
const initialFormName = 'initial';
initialInput.append(createTextInputPanel(initialFormName));
initialInput.style.display = (mode === '')? 'none': 'flex';

const initSubmitBtn = document.getElementById(`${initialFormName}-input-submit-btn`);
// const initInputNextBtn = document.getElementById(`${initialFormName}-input-next-btn`);
const initInputFormId = initialFormName + '-input-form';
const initInputForm = document.getElementById(initInputFormId);

initInputForm.onchange = () => isFilled(initInputFormId)? initSubmitBtn.disabled = false: null;
initSubmitBtn.onclick = () => {
  let inputs = initInputForm.elements;
  Object.keys(inputs).forEach(id => subjectData[inputs[id].name] = inputs[id].value);
  initSubmitBtn.disabled = true;
  disableFormInputs(initInputFormId);
  // initInputNextBtn.disabled = false;
  hide("core-learn-form-div");
  showNext("core-gen-div")
}
// initInputNextBtn.onclick = () => {
//   hide("core-learn-form-div");
//   showNext("core-gen-div")
// }
initialInput.style.display = (mode==='dev')? 'flex': 'none';
// initialInput.style.display = 'none';

/** Core: generalization tasks */
let genDiv = document.getElementById("core-gen-div");
for(let i = 0; i < genConfigs.length; i++ ) {
  const taskConfig = genConfigs[i];
  const config = {
    'taskId': taskConfig[0],
    'agent': taskConfig[2],
    'recipient': taskConfig[3],
  };
  const taskId = config.taskId;
  let display = (mode==='dev'|i===0)? 'flex': 'none';

  let box = createCustomElement("div", "box", `box-${taskId}`);
  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);

  let taskNum = createText('h3', `${i+1}/${genConfigs.length}:
    This active stone will turn this inactive stone into ...?`);
  let incentive = createText('h4', `$.10 bonus for each guess you make correctly (according to the true hidden powers of the stones)`)
  taskBox.append(taskNum);
  taskBox.append(incentive);

  let displayDiv = createCustomElement("div", "display-div", `${taskId}-display-div`);
  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  displayBox = createInitStones(config, displayBox);

  displayDiv.append(displayBox);
  displayDiv.append(createAnswerComposer(config));

  taskBox.append(displayDiv);
  box.append(taskBox);
  genDiv.append(box);
  box.style.display = display;

  const selectionForm = document.getElementById(`${taskId}-selection-form`);
  const confirmBtn = document.getElementById(`${taskId}-confirm-btn`);
  // const selNextBtn = document.getElementById(`${taskId}-selection-next-btn`);

  selectionForm.onchange = () =>
    composeSelection(`${taskId}-selection-svg`, `${taskId}-selection-form`, `${taskId}-confirm-btn`);
  confirmBtn.onclick = () => {
    let inputs = selectionForm.elements;
    let selection = {};
    Object.keys(inputs).forEach(id => selection[inputs[id].name] = inputs[id].value);
    let stoneCode = parseInt(selection.shape.slice(1,) + selection.color.slice(1,));
    trialData['result'].push(stoneCode);
    disableFormInputs(`${taskId}-selection-form`);
    confirmBtn.disabled = true;
    // selNextBtn.disabled = false;
    if (i < genConfigs.length-1) {
      (mode!=='dev')? hide(`box-${taskId}`): null;
      showNext(`box-gen-${padNum(i+2)}`)
    } else {
      showNext("core-gen-form-div")
    }
  }
  // selNextBtn.onclick = () => {
  //   (mode!=='dev')? hide(`box-${taskId}`): null;
  //   if (i < genConfigs.length-1) {
  //     showNext(`box-gen-${padNum(i+2)}`)
  //   } else {
  //     showNext("core-gen-form-div")
  //   }
  // }
}
genDiv.style.display = (mode==='dev')? 'flex': "none";
// genDiv.style.display = 'none';

/** Core: final input form */
let finalInput = document.getElementById("core-gen-form-div");

let fiBox = createCustomElement("div", "input-div", `final-input`);
let fiButtonGroup = createCustomElement("div", "button-group", `final-button-group`);
fiButtonGroup.append(createBtn(`final-input-submit-btn`, "OK", false));

let finalForm = createCustomElement("form", "input-form", `final-input-form`);
finalForm.innerHTML = `
  <p><b>Has your impression about how these mysterious stones work changed?</b>
    <select name="final_change" id="final_change" class="input-rule">
      <option value="--" SELECTED>
      <option value="1">Yes</option>
      <option value="0">No</option>
    </select>
  </p>`

finalInput.append(finalForm);
finalInput.append(fiButtonGroup);

const fiBtn = document.getElementById('final-input-submit-btn');
finalForm.onchange = (e) => fiBtn.disabled = (e.target.value === '--')? true: false;
fiBtn.onclick = () => {
  subjectData['final_changed'] = document.getElementById('final_change').value;
  hide("tasks")
  showNext("debrief", "block")
}

// finalInput.append(createTextInputPanel(finalFormName));
// finalInput.style.display = (mode === '')? 'none': 'flex';

// const finalSubmitBtn = document.getElementById(`${finalFormName}-input-submit-btn`);
// // const finalInputNextBtn = document.getElementById(`${finalFormName}-input-next-btn`);
// const finalInputFormId = finalFormName + '-input-form';
// const finalInputForm = document.getElementById(finalInputFormId);

// finalInputForm.onchange = () => isFilled(finalInputFormId)? finalSubmitBtn.disabled = false: null;
// finalSubmitBtn.onclick = () => {
//   let inputs = finalInputForm.elements;
//   Object.keys(inputs).forEach(id => subjectData[inputs[id].name] = inputs[id].value);
//   finalSubmitBtn.disabled = true;
//   disableFormInputs(finalInputFormId);
//   // finalInputNextBtn.disabled = false;
//   hide("tasks")
//   showNext("debrief", "block")
// }
// finalInputNextBtn.onclick = () => {
//   hide("tasks")
//   showNext("debrief", "block")
// }
finalInput.style.display = (mode==='dev')? 'flex': "none";

/** Debrief page */
const doneBtn = document.getElementById('done-btn');
const debriefForm = document.getElementById('postquiz');

debriefForm.onchange = () => {
  isFilled('postquiz')? doneBtn.disabled = false: null;
}
doneBtn.onclick = () => {
  let inputs = debriefForm.elements;
  Object.keys(inputs).forEach(id => subjectData[inputs[id].name] = inputs[id].value);

  const end_time = new Date();
  let token = generateToken(8);

  let clientData = {};
  clientData.subject = subjectData;
  clientData.subject.condition = cond;
  clientData.subject.date = formatDates(end_time, 'date');
  clientData.subject.time = formatDates(end_time, 'time');
  clientData.subject.instructions_duration = start_task_time - start_time,
  clientData.subject.task_duration = end_time - start_task_time,
  clientData.subject.token = token;
  clientData.trials = trialData;

  /** Give feedback */
  const truths = genConfigs.map(c => c[4])
  const predicted = trialData.result.slice(learnConfigs.length,);
  let correct = 0;
  truths.forEach((t, i) => (t===predicted[i])? correct+=1: null);
  clientData.subject.correct = correct;

  if (mode === 'flask') {
    fetch(root_string, {
        method: 'POST',
        body: JSON.stringify(clientData),
    })
    .then(() => showCompletion(token, correct))
    .catch((error) => console.log(error));
  } else {
    showCompletion(token, correct);
    console.log(clientData);
    // download(JSON.stringify(clientData), 'data.txt', '"text/csv"');
  }
};
