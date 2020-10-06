
/** This script generates simuli configuration for Experiment A1-A4:
 * Conditions: 2 x 2 design:
 *  A1 & A2 ground truth: edges(A)+1, shades(R)+1;
 *  A3 & A4 ground truth: edges(R)+1, shades(A)+1;
 *  A1 & A3: fix constant Agent, vary Recipient systematically;
 *  A2 & A4: fix constant Recipient, vary Agent systematically;
 * Object representation:
 *  Two digits integer:
 *    tens digit for number of edges (3 - triangle, 4 - square, 5, 6, 7);
 *    units digit for shading degree (1 - very light, 2 - medium, 3 - dark, 4 - v. dark).
 *  Eg. 31 for very_light triangle
 * Usage:
 *  Run this script _four_ times to setup four experiment configurations.
 *  Name each configuration w.r.t experiment condition.
 *  Do not change the generated config object name - see how task.js consumes it.
 */

let config = [];
const condition = 'all';

const fixed_obj = 42;
const varied_obj = [ 31, 53, 62, 33, 52, 61 ].sort();

const contrast_fixed_obj = [ 32, 43, 61, 42 ].sort();
const contrast_varied_obj = [ 32, 41, 51, 63 ].sort();

/** Generate experiment configs */
if (condition=='all') {
  // - see setup.html
  for (let i = 1; i < 5; i++) {
    let cond = 'A' + i;
    config = config.concat(getExpConfig(cond));
  }
} else {
  // Save it to a `config_[condition].js` file
  config = getExpConfig(condition)
}

// console.log(config);
// console.log(JSON.stringify(config))

/** Functions that do the real job */
function getExpConfig (condition) {
  let configArr = [];

  // Learning trials
  const rule = (condition[1] > 2) ? 'AB' : 'AA';
  if (condition[1] % 2 == 1) {
    // Fix Agent, vary Recipient
    varied_obj.forEach((r, idx) => {
      configArr.push(formatConfig(idx+1, condition, 'learn', fixed_obj, r, rule))
    })
  } else {
    varied_obj.forEach((r, idx) => {
      configArr.push(formatConfig(idx+1, condition, 'learn', r, fixed_obj, rule))
    })
  }

  // Generalization trials
  let pairs = [];
  const gen_agents = (condition[1] % 2 == 1)? contrast_fixed_obj: contrast_varied_obj;
  const gen_recipients = (condition[1] % 2 == 1)? contrast_varied_obj: contrast_fixed_obj;
  gen_agents.forEach(a => {
    gen_recipients.forEach(r => pairs.push([a, r]))
  })
  pairs.forEach((pair, idx) => {
    configArr.push(formatConfig(idx+1, condition, 'gen', pair[0], pair[1], rule))
  })

  return configArr
}
function formatConfig (idx, group, phase, agent, recipient, rule) {
  const padNum = (num) => num.toString().padStart(2, '0');
  let config = {};
  config['sid'] = `${group}-${phase}-${padNum(idx)}`
  config['group'] = group;
  config['phase'] = phase;
  config['trial'] = idx,
  config['agent'] = agent;
  config['recipient'] = recipient;
  config['result'] = fetchResult(agent, recipient, rule);
  return config
}
function fetchResult (agent, recipient, rule) {
  const readShades = (obj) => obj % 10
  const readEdges = (obj) => Math.floor(obj / 10)
  switch (rule) {
    case "AA":
      return (readEdges(agent)+1) * 10 + (readShades(recipient)+1);
    case "AB":
      return (readEdges(recipient)+1) * 10 + (readShades(agent)+1);
    default:
      return 0
  }
}
