
console.log("Hi")

/** Prep data */
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
  genConfigs.push([taskId, sid, cfg.agent, cfg.recipient])
})

let trialData = {
  "phase": [],
  "tid": [],
  "sid": [],
  "agent": [],
  "recipient": [],
  "result": [],
  "confidence": [],
};
learnConfigs.forEach(c => {
  trialData['phase'].push('learn');
  trialData['tid'].push(c[0]);
  trialData['sid'].push(c[1]);
  trialData['agent'].push(c[2]);
  trialData['recipient'].push(c[3]);
  trialData['result'].push(c[4]);
  trialData['confidence'].push(0);
})
genConfigs.forEach(c => {
  trialData['phase'].push('learn');
  trialData['tid'].push(c[0]);
  trialData['sid'].push(c[1]);
  trialData['agent'].push(c[2]);
  trialData['recipient'].push(c[3]);
})

const start_time = Date.now();
let start_task_time = 0;

/** Showcase contents */

/** Core: learn tasks */
const coreLearnDiv = document.getElementById("core-learn-div");
for(let i = 0; i < learnConfigs.length; i++ ) {
  const taskConfig = learnConfigs[i];
  console.log(taskConfig)

  const taskId = taskConfig[0];
  const agent = taskConfig[2];
  const recipient = taskConfig[3];
  const result = taskConfig[4];

  let box = createCustomElement("div", "box", `box-${taskId}`);
  let taskBox = createCustomElement("div", "task-box", `taskbox-${taskId}`);

  let taskNum = createText('h2', i+1 + '/' + learnConfigs.length);
  taskBox.append(taskNum);

  let displayBox = createCustomElement("div", "display-box", `${taskId}-display-box`);
  // displayBox = createInitStones(config, displayBox);

  const buttonGroup = createCustomElement("div", "button-group-vc", `${taskId}-button-group`);
  buttonGroup.append(createBtn(`${taskId}-play-btn`, "Play", true));
  buttonGroup.append(createBtn(`${taskId}-next-btn`, "Next", false));

  taskBox.append(displayBox);
  taskBox.append(buttonGroup);
  box.append(taskBox);
  coreLearnDiv.append(box);
}


/** Core: initial input form */

/** Core: generalization tasks */

/** Core: finail input form */
