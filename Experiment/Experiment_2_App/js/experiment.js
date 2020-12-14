// this version: fix random disappearance of feedback text
// it's because the pinging I added also uses a variable called response
// and text disappears when response doesn't equal 0 or 1
// what I did to fix this: I just changed all instances of the non-AJAX "response" variable name to "resp"

//////////////////////////////////////////////////////////////////////////////
// OVERVIEW

//  1 = IRB
//  2 = instruction 1
//  5 = comprehension
//  6 = instruction 2
//  7 = trial blank  (millis, start with 6)
//  8 = trial        (millis, start with 6)
//  9 = feedback     (millis, start with 6)
// 10 = instruction 3
// 18 = debrief (last page)

//////////////////////////////////////////////////////////////////////////////
// DEFINE GLOBAL VARIABLES

var phase = -3;  // = -3 for running experiment, can change for development

/////////////////////////////////////////
// PARAMS TO GET FROM DATASTORE:

var entityID;  // ID property of the entity "Chain" or "Individual" in Google Datastore
var condition; // "I" = individual, "C" = cultural
var c_code;
var frequency_condition; // (called "distribution" in the datastore) "L" = black most frequent, "R" = white most frequent, "U" = uniform
var chain;
var generation;
var transmit; // becomes variable "correct_labels"
var parent;
var jointime_webapp2; // the UTC time webapp2 assigns to the entity's "status" when a slot is taken
// some of these will come in as "NA", depending on whether the condition is "I" or "C"

/////////////////////////////////////////

var next_button, back_button;
var notebook_pic, shell_pic, alien1_pic, alien2_pic, alien3_pic, alien4_pic;
var check1, check2, check3, allChecked;
var leftmargin, topmargin, bodymargin;
var timestamp;
var comprehension_warning; // true when Next button is clicked and answers are wrong or left blank
var proceed; // boolean for defaulting all html buttons to proceed. proceed = false disables the proceed function
var answer1, answer2, answer3; // where the answers to the comprehension questions are stored
var comprehension_tally; // counts how many times participant submitted incorrect comprehension questions
var tally_limit; // the submission number that kicks them out of the experiment 
var completion_code, bootout_code, b_code;
var expedition_duration;
var trial_blank;
var round_counter;
var train_trial_counter, test_trial_counter;
var number_train_trials, number_test_trials, number_rounds;
var trial_blank_time, stim_only_time, feedback_time;
var ok_pushed;
var word0, word1;
var test_right, test_left, test_ok; // test booleans for testButton(). test_right = T means right test button is currently selected
var right_testbutton_x, right_testbutton_y, left_testbutton_x, left_testbutton_y, testbutton_width, testbutton_height;
var test_ok_x, test_ok_y, test_ok_width, test_ok_height, test2_ok_y;
var feedback;
var object_ID, object_color, object_frequency, object_label;
var R, G, B; // RGB values that replace object_color (greyscale) values
// variables for saving data
var trainset_ID;
var trainset_ID_string, test_ID_string; // join() trainset_ID into this per round
var trainorders_string; // all trainset_ID_string saved in here, per round
var testorders_string; // all test_ID_string saved in here, per round
var trainresponses, testresponses; // save which labels were chosen here, binary array, 0 = word0 = left, 1 = word1 = right.
var trainresponses_string; // all train responses saved in here, per round (just final response)
var testresponses_string; // all test responses saved in here, per round (just final response)
var data_string; // where all of the data per participant is saved, in string format, to send via url
// word label stuff
var correct_labels; // binary array coding label mapping (0 or 1) to object_ID, where index = object ID

// response also gets used in my AJAX requests! So I had to change this.
//var response; // the final response the participant made on the trial, 0 = word0 = left, 1 = word1 = right
var resp;  // the final response the participant made on the trial, 0 = word0 = left, 1 = word1 = right

// uncertainty measures
var response_change;  // # times participant changed answer on the current trial, 0 to whateva
var trainresponse_changes, testresponse_changes; // an array of all response changes per trial for the whole round
var RT, RTs; // milliseconds since trial start to first click in a response box, same plural use as above
var experiment; // this is a fixed variable for all of the participants (for google datastore purposes)
var condition; // "I" or "C", I = individual, C = cultural
var converged; // boolean, true = participant's testing responses were the same twice in a row
var score; // this is where the current score (number test responses = correct label) per round is stored - overwritten each round
var score_per_round; // integer array, concatenates score per round - can be summed to get total score
var total_score; // the sum of score_per_round
var callback; // callback variable set to true when loadData() GET request is successful

// milliseconds elapsed since experiment loaded and:
var consent_millis;       // the consent phase is completed (assigned when participant leaves phase -3)
var callback_millis;      // the loadData() request is a success (assigned when participant leaves phase -1)
var comprehension_millis; // comprehension questions are successfully passed (assigned when leaves phase 5)
var instruction_millis;   // Instructions2 is passed (assigned when leaves phase 6)
var round_begin;          // Round 1: same as instruction_millis (assigned when leaves phase 6 - phase 7 is next)
                          // Rounds 2+: (assigned when leaves phase 15 - phase 7 is next)
var round_end;            // (assigned right before program goes to phase 13)
// when final_round = true, round_end is the length of the whole experiment (minus reading Instructions5 & Debrief)

// durations - these are what get saved (the millis section above are just for internal program use)
var duration_consent;       // ms between experiment loaded and consent was given
var duration_callback;      // ms between consent given and callback received
var duration_comprehension; // ms spent on Instructions1 AND comprehension section
var duration_instruction;   // ms spent on Instructions2 (that's one single page)
var duration_experiment;    // ms between experiment load and the current round ended
var whole_experiment;       // = duration_experiment rounded to the nearest minute, calculated ONLY for last round
var jointime_user;          // time on user's computer right after loadData() is called (jointime_webapp2 is assigned in loadData())
var jointime_user_UTC;      // time on user's computer right after loadData() is called - converted to UTC time

var session_code;   // generate a random unique code for this user
                    // problem is c_code is not unique in the Individual condition - it's a design flaw and this is the workaround.

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function preload() {

  // load all images
  notebook_pic = loadImage("assets/notebook.jpg");
  arrow = loadImage("assets/arrow.png");
  shell_pic = loadImage("assets/shell.png");
  alien1_pic = loadImage("assets/alien1.png"); // Instructions 1
  alien2_pic = loadImage("assets/alien2.png"); // feedback
  alien3_pic = loadImage("assets/alien3.png"); // be right back
  alien4_pic = loadImage("assets/alien4.png"); // I'm back

  // https://p5js.org/reference/#/p5/preload
  // "Nothing besides load calls (loadImage, loadJSON, loadFont, loadStrings, etc.) should be inside the preload function."
  // "If a preload function is defined, setup() will wait until any load calls within have finished."

}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function setup() {
  createCanvas(800, 600);
  textFont("Helvetica");
  noStroke(); fill(0);
  textAlign(LEFT); textSize(20);
  imageMode(CENTER,CENTER);
  
  ///////////////////////////////////////////////////////////////////////
  // SET MAIN VARIABLES
  
  experiment_started = Date();
  //console.log(experiment_started);
  
  ping_rate = 60000; // 60000 ms = 1 minute
  // throughout the life of the experiment, ping the server every x minutes (to say user is still there)
  setInterval(sendVitals,ping_rate) // need to put this in setup(), not draw() - coz just define once.
  //print(entityID)     // not assigned yet
  //print(session_code) // not assigned yet
  //print(condition)    // not assigned yet
  //print(frequency_condition)  // not assigned yet
  //print(phase) // is assigned

  // !!!
  feedback = true; // delete after development - gets set correctly below (check correctness)

  //if (int(random(0,2)) == 0) { condition = "I"; } else { condition = "C"; } // set main condition randomly
  //console.log("running setup()");

  experiment = "category_change_2"

  // IMPORTANT: these 2 arrays must be of equal length
  words0 = ["buv","dap","pon","vit","seb","gos"];
  words1 = ["kal","mig","fud","lem","nuk","tef"];
  
  chooseLabels(); // randomly choose one pair from word0 & word1
  
  object_ID = ownRange(10); // ex: ownRange(10) = [0,1,2,3,4,5,6,7,8,9];  the IDs of the ten objects, ranging from #0 to #9
  //object_color = [25,50,75,100,125,150,175,200,225,250]; // hardcoded knowing # objects
  // replace object_color with R,G,B values
  // function fill() takes one greyscale number, ex: fill(150), or RGB values like this fill(R,G,B), ex: fill(100,0,155)
  R = [100,89,78,67,56,45,34,23,12,1];
  G = [0,0,0,0,0,0,0,0,0,0];
  B = [155,166,177,188,199,210,221,232,243,254];

  max_rounds = 8;   // 8
  number_train_trials = 30; // 30  (must be the sum of object_frequency)
  number_test_trials = 10; // 10

  callback = false; // callback variable set to true when loadData() get request is successful
  
  // gets set in buildTrainset()
  trainset_ID = [];
  
  // gets set in buildTestset()
  testset_ID = [];
  
  tally_limit = 3 // ex: 3 = on the third submission of wrong comprehension answers, participant is booted out of experiment.

  trial_blank_time = 1000; // 1000 milliseconds
  stim_only_time = 0; // 3000 
  feedback_time = 3000; // 3000  //ok button appears after this, then participants can click it whenever

  // TODO REMOVE BOOTOUT CODE STUFF - just quit the experiment on them.
  b_code = "gxjbk358"
  bootout_code = createElement('h2',b_code);
  
  session_code = "s"+nf(int(random(99999999,10000000))); // unique code for this session
  //console.log(session_code);
  
  last_click = "NA";
  response_change = 0; // how many times the participant changes their answer (on the current trial) before cliking ok.
  
  leftmargin = 20; // for text placement
  topmargin = 40;
  bodymargin = 100;
  
  v_adjust = 60; // vertical adjustment for the whole confirmation questions form (not including title area)
  h_adjust = 30; // horizontal adjustment of just the radio buttons part
  
  right_testbutton_x = 600;
  right_testbutton_y = 500; 
  left_testbutton_x = 200; 
  left_testbutton_y = 500;
  testbutton_width = 150; 
  testbutton_height = 100;
  
  test_ok_x = width/2;
  test_ok_y = 500;
  test2_ok_y = 500-50;
  test_ok_width = 32;
  test_ok_height = 28;
  
  // just for debugging to print in draw()
  test_left = false;
  test_right = false;
  resp = "na";
  
  ///////////////////////////////////////////////////////////////////////
  // DO NOT CHANGE these variables!
  // (program may malfunction)
  
  //phase = 0;  // turn on for final version
  proceed = true;  // sets to false when a question is answered incorrectly or left unanswered.
  comprehension_tally = 0; // # wrong answer submissions.
  
  round_counter = 1; // increases after a round is completed
  train_trial_counter = 0; // 1 means show 1st object in trainset, gets increased in the blank right before each trial, so must init at 0.
  test_trial_counter = 0;  // 1 means show 1st object in testset, gets increased in the blank right before each trial, so must init at 0.
  // NOTE: python indeces start at 0 so I need to use "trainset_ID[train_trial_counter-1]" if the counter = 1 and I want the 1st element of trainset_ID
  
  test_ok= false; // only needed for printing value to screen for debugging
  
  answer1 = "NA";
  answer2 = "NA";
  answer3 = "NA";
  
  converged = false; // individual condition: this may update to true, chain condition: this will never update to anything else.
  
  trainorders_string = "train_order-"; // initialize the string.  rounds separated by semicolons, in order of round
  testorders_string = "test_order-";
  trainresponses = []; // reset to empty after each round
  testresponses = [];
  trainresponses_string = "train_responses-";
  testresponses_string = "test_responses-";
  trainresponse_changes = [];
  testresponse_changes = [];
  trainresponse_changes_string = "train_changes-";
  testresponse_changes_string = "test_changes-";
  train_RTs = [];
  test_RTs = [];
  train_RTs_string = "train_RTs-";
  test_RTs_string = "test_RTs-";
  correct_labels_string = "label_map-"
  score_per_round = []; // need to push to this, so needs to be defined in advance
  
  RT_begin = "NA";
  RT_end = "NA";
  RT = "NA";
  RT_obtained = false;

  
  ///////////////////////////////////////////////////////////////////////
  // html buttons
  
  back_button = createButton('Back');
  back_button.position(80,500+26);
  back_button.style('font-size: 30px');
  back_button.mouseClicked(prev_phase);
  
  next_button = createButton('Next');
  next_button.position(643,500);
  next_button.style('font-size: 30px');
  next_button.mouseClicked(next_phase);
  
  next_mid_button = createButton('Next');
  next_mid_button.position(width/2-50,500);
  next_mid_button.style('font-size: 30px');
  next_mid_button.mouseClicked(next_phase);
  
  submit_button = createButton('Submit');
  submit_button.position(643,500+26);
  submit_button.style('font-size: 30px');
  submit_button.mouseClicked(next_phase);
  
  // comprehension question 1, radio button
  div1 = createDiv('').size(100,100);
  div1.style('background-color: white; width: 400px;');
  div1.position(300+h_adjust,85+v_adjust);
  div1.html('<body><form action=""> \
  <input type="radio"> size <br> \
  <input type="radio"> shape <br> \
  <input type="radio"> texture <br> \
  <input type="radio"> color </form></body>');
  radio1 = createRadio();
  radio1.option(" ", 1);
  radio1.option(" ", 2);
  radio1.option(" ", 3);
  radio1.option(" ", 4);
  radio1.style('width','10px');
  radio1.position(300+h_adjust,85+v_adjust);
  
  // comprehension question 2, radio button
  div2 = createDiv('').size(100,100);
  div2.style('background-color: white; width: 400px;');
  div2.position(300+h_adjust,205+v_adjust);
  div2.html('<body><form action=""> \
  <input type="radio"> a doctor <br> \
  <input type="radio"> a biologist <br> \
  <input type="radio"> a space explorer <br> \
  <input type="radio"> an oceanographer </form></body>');
  radio2 = createRadio();
  radio2.option(" ", 1);
  radio2.option(" ", 2);
  radio2.option(" ", 3);
  radio2.option(" ", 4);
  radio2.style('width','10px');
  radio2.position(300+h_adjust,205+v_adjust);
  
  // comprehension question 3, radio button
  div3 = createDiv('').size(100,100);
  div3.style('background-color: white; width: 400px;');
  div3.position(300+h_adjust,325+v_adjust);
  div3.html('<body><form action=""> \
  <input type="radio"> to teach you all about life on Zorg <br> \
  <input type="radio"> to help you find the shells <br> \
  <input type="radio"> to help you learn the labels for the shells <br> \
  <input type="radio"> to teach you how to cook the shells in a stew </form></body>');
  radio3 = createRadio();
  radio3.option(" ", 1);
  radio3.option(" ", 2);
  radio3.option(" ", 3);
  radio3.option(" ", 4);
  radio3.style('width','10px');
  radio3.position(300+h_adjust,325+v_adjust);
  
  expedition_button = createButton('Begin the expedition!');
  expedition_button.position(270,500);
  expedition_button.style('font-size: 30px');
  expedition_button.mouseClicked(next_phase);
  
  onit_button = createButton('Ok, I\'m on it!');
  onit_button.position(300,500);
  onit_button.style('font-size: 30px');
  onit_button.mouseClicked(next_phase);
  
  ready_button = createButton('Ok, I\'m ready!');
  ready_button.position(300,500);
  ready_button.style('font-size: 30px');
  ready_button.mouseClicked(next_phase);
  
  score_button = createButton('Get my code');
  score_button.position(300,500);
  score_button.style('font-size: 30px');
  score_button.mouseClicked(next_phase);

  // create a link to down pdf of consent page 
  //consent_link = createA('http://p5js.org/', 'Consent Page');
  //score_button.position(300,500);


  // html buttons END
  ///////////////////////////////////////////////////////////////////////
  
  // initialize all html buttons to hidden (not on screen)
  back_button.hide();
  next_button.hide();
  next_mid_button.hide();
  submit_button.hide();
  div1.hide() ,div2.hide(), div3.hide();
  radio1.hide(), radio2.hide(), radio3.hide();
  expedition_button.hide();
  onit_button.hide();
  ready_button.hide();
  score_button.hide();
  //consent_link.hide(); // not an html button, but it is a DOM element.
  
  bootout_code.hide();
  
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function draw() {
  background(255);

  // Main Program Flow (divided into phases)
  
  if (phase > 1) {
    push(); fill(255); stroke(0); strokeWeight(2); rect(1,1,width-3,height-3); pop();
  }
  
  ///ttt
  //text(resp,30,30);
  //text("phase "+str(phase),300,30);
  //text("round "+str(round_counter),400,30);
  //text("train "+str(train_trial_counter),500,30);
  //text("test "+str(test_trial_counter),500,60);
  
  //text("proceed "+str(proceed),300,20);
  //text("feedback "+str(feedback),width/2,90);
  
  //text(comprehension_tally,30,70);
  //text(timestamp,30,70);
  //text(millis(),30,100);
  //text(test_ok,400,30);
  //text(test_left,200,580);
  //text(test_right,600,580);
  //text(resp,400,580);
  //text("# response changes: "+str(response_change),30,70);
  //text("last click: "+str(last_click),30,90);
  //text(RT_begin,30,130);
  //text(RT_end,30,150);
  //text(RT,30,170);
  //print(test_trial_counter);
  
  // what to put exclusively here:
  // functions
  // all events that hide or show html buttons
  // all routing to different phases, besides what's done in next_phase()

  if ( phase === "abort" ) {
    abortExperiment();
    if ( comprehension_tally === tally_limit ) { phase = "byebye"; }
  }


  if ( phase === "byebye" ) {
    displayBootout();
    
    back_button.hide();
    next_button.hide();
    next_mid_button.hide();
    submit_button.hide();
    div1.hide() ,div2.hide(), div3.hide();
    radio1.hide(), radio2.hide(), radio3.hide();
  }
  
  // Consent Page
  if ( phase === -3 ) {
    displayConsent();
    
    back_button.hide();
    if ( allChecked === true ) { next_button.show(); }
    
    consent_millis = millis();
  }

  // load training data (and join chain)
  if ( phase === -2 ) {
    //console.log(phase);
    next_button.hide();
    
    loadData(); // jointime_webapp2 is called here in an AJAX request, so it takes a couple moments to process
    
    user_date_object = new Date(); // this will probably register an earlier time than jointime_webapp2 (diff = AJAX processing time)
    jointime_user = user_date_object.toString();
    jointime_user_UTC = myUTC(user_date_object);
    
    phase = -1;
  }

  // wait until callback says the data was loaded
  if ( phase === -1 ) {
    // if there is a load issue or everything is full, the experiment stays on this page until user closes it
    // so don't print anything to this screen
    //console.log(phase);
    if ( callback === true ) { // "true" means the get request was a success (i.e. data for a slot was returned)
      callback_millis = millis();
      phase = 0;
    }
  }

  // zzz
  // setup() continued
  if ( phase === 0 ) { //  had to move some stuff from setup() to here - everything that required loadData() first
    //console.log(phase);
    
    completion_code = createElement('h2',c_code+session_code); // turn "c_code" into an html element, so users can copy and paste it
    completion_code.hide(); // then immediately hide it - normally would hide it during setup() but ah well.

    if (frequency_condition == "U") { object_frequency = [3,3,3,3,3,3,3,3,3,3]; }
    if (frequency_condition == "R") { object_frequency = [1,1,1,1,2,2,3,4,5,10]; }  
    if (frequency_condition == "L") { object_frequency = [10,5,4,3,2,2,1,1,1,1]; } // hardcoded knowing # objects

    buildTrainset(); // for round 1 only
    buildTestset(); // for round 1 only

    //print("trainset_ID:");
    //print(trainset_ID);

    // set variable "correct_labels" (the label to object mapping)
    if (condition == "I") { // there are no correct_labels - create from scratch
      //print("create initial label map with randoMapper()");
      randoMapper("equal"); // type = "equal" or "binomial"  !!! the number of objects must be even for equal to work.
    }
    if (condition == "C") { // correct_labels were loaded already, check if they = init, and if so, generate from scratch.
      if (correct_labels == "init") { // datastore: transmit = init
        //print("create initial label map with randoMapper()");
        //print(correct_labels);
        randoMapper("equal");
        //print(correct_labels);
      } else { // datastore: transmit = "1,1,1,0,1,0,0,0,0,1" - some set of correct_labels in string format
        //xxx
        correct_labels = JSON.parse("[" + correct_labels + "]"); // convert from string to list (correct_labels must be a list)
        //print(correct_labels);
      }

      max_rounds = 1; // just in the chain condition, override max_rounds and set to 1.
    }
    
    phase = 1;

    ///////////////////////////////////////////////////////////////////////
    // DEMO ONLY (rewrites whateva's above)
    //object_frequency = [1,1,1,1] // will use the first 4 colors
    //number_train_trials = 4;
    //number_test_trials = 4; // must = N objects for whole program to work
    //object_ID = ownRange(object_frequency.length);
    
    //object_frequency = [1,1,1,1,1,1,1,1,1,1] // will use the first 4 colors
    //number_train_trials = 10;
    //number_test_trials = 10; // must = N objects for whole program to work
    //object_ID = ownRange(object_frequency.length);

    //object_frequency = [2,2,2,2] // will use the first 4 colors (must sum to n trials)
    //number_train_trials = 8;
    //number_test_trials = 4; // must = N objects for whole program to work
    //object_ID = ownRange(object_frequency.length);
    ///////////////////////////////////////////////////////////////////////
  }

  // Consent Page
  if ( phase === 1 ) {
    // Consent Page moved from here to phase -3
    // CONSENT REVERT 1 of 2: put the current phase -3 code back here, and change experiment start to -2
    phase = 2;
  }
  
  // Instructions 1 Page
  if ( phase === 2 ) {
    displayInstructions1();
    
    back_button.hide();
    next_button.hide();
    next_mid_button.show();
    submit_button.hide();
    div1.hide() ,div2.hide(), div3.hide();
    radio1.hide(), radio2.hide(), radio3.hide();
    
  }
  
  // routing
  // forward navigation
  if ( phase === 3 ) {
    proceed = false;  // disable Next button on Comprehension Page
    comprehension_warning = false; // turn off comprehension "wrong answers" text (for when back navigation occurs)
    phase = phase+2; // to comprehension phase
  }
  
  // routing
  // back navigation
  if ( phase === 4 ) {
    proceed = true; // enable Next button on Instructions1 Page
    comprehension_warning = false;  // turn off comprehension "wrong answers" text (because back navigation just occurred)
    phase = phase-2; // to instuctions1 phase
  }
  
  // Comprehension Page
  if ( phase === 5 ) {
    displayComprehension();
    // next button is disabled until all comprehension questions are answered correctly
    // back button goes back one phase (to phase 3, which re-directs to Instructions1 Page)
    
    // see next_phase() for variable assignments when answers are correct
    
    if ( comprehension_tally === tally_limit ) {
      phase = "abort";
    }
    
    back_button.show();
    next_mid_button.hide();
    submit_button.show();
    div1.show(), div2.show(), div3.show();
    radio1.show(), radio2.show(), radio3.show();
    
    comprehension_millis = millis();
  }
  
  // Instructions 2 Page
  if ( phase === 6 ) {
    displayInstructions2();
    
    back_button.hide();
    next_button.hide();
    submit_button.hide();
    div1.hide() ,div2.hide(), div3.hide();
    radio1.hide(), radio2.hide(), radio3.hide();
    expedition_button.show();
    
    instruction_millis = millis();
    round_begin = millis();
  }
  
  // train trial blank
  if ( phase === 7 ) {
    displayTrialBlank();
    
    expedition_button.hide();
    ready_button.hide();
  }
  
  // trial
  if ( phase === 8 ) {
    displayTrial();
    // record response on ok button press in mousePressed()
  }
  
  // feedback
  if ( phase === 9 ) {
    displayFeedback();
  }
  
  // Instructions 3 Page
  if ( phase === 10 ) {
    displayInstructions3();
    feedback = false; // and turn feedback off for the test trials
    
    onit_button.show();
  }
  
  // test trial blank
  if ( phase === 11 ) {
    displayTrialBlank();
    
    onit_button.hide();
  }
  
  if ( phase === 12 ) {
    displayTrial();
    // record response on ok button press in mousePressed()
  }
  
  // save round data, evaluate convergence
  if ( phase === 13 ) {
    
    ///////////////////////////////////////
    // everything in this chunk must occur in this order:
    saveLastRound(); // save last round's: train order, test order, train responses, test responses, correct_labels.
    
    // TO DO doesn't save the very last correct labels map - fix that - think that's fixed now.
    
    last_round_labels = deepCopyArray(correct_labels); // deep copy correct_labels for individual convergence comparison

    testMapper(); // use current testset_ID & testresponses to re-write correct_labels

    score = correctsChecker(last_round_labels,correct_labels); // get score of current round
    score_per_round.push(score)  // append to score history over all rounds
    //print("score per round");
    //print(score_per_round);

    if ( condition === "I" ) { 
      convergenceChecker(last_round_labels,correct_labels); // check to see if participant's testing responses are the same as last round
      //print("convergence? "+str(converged));
    }
    // TO DO figure out how to convergence check for IL condition

    /// send all data from this round to server.py
    sendLastRound();
    
    // reset response variables to empty for next round:
    trainresponses = []; 
    testresponses = []; 
    trainresponse_changes = [];
    testresponse_changes = [];
    train_RTs = [];
    test_RTs = [];
    
    // these resest trainset_ID & testset_ID for next round
    buildTrainset(); // randomize the trial order per stimulus (base frequencies remain the same)
    buildTestset();
    ///////////////////////////////////////
    
    if ( converged === true ) {
      phase = 16; // no more rounds, go to Instructions 5
    }
    
    if ( converged === false ) {
      // needed to wrap this in converged === false to get the block above to execute when true 
      // (setting phase to other thrue conditions overwrote the assignment above).
      
      // if no convergence,
      // check if the total number of rounds have been completed.
      // (if only the first round has been completed, round_counter currently reads 1 - meaning round 1 is in progress)
      if ( round_counter === max_rounds ) { // if the max number of rounds are complete,
        phase = 16; // go to Instructions 5 & end experiment
      } else {
        phase = 14; // go to Instructions 4 (to start new round)
        feedback = true; // set back to training
        train_trial_counter = 0;  // reset for new round
        test_trial_counter = 0;   // reset for new round
      }
    }
    
    // now increase the round counter
    round_counter++; // ex: when the first round is completed, this increases to 2
    //print("ROUND "+str(round_counter));
  
    ready_button.hide();
    score_button.hide();
    
    // TO DO make sure the data saves in a good format (ex: semicolons on single round?)
    
  }
  
  // Instructions 4 - next round
  if ( phase === 14 ) {
    
    proceed = true;
    
    displayInstructions4();
    
    ready_button.show();
  }
  
  if ( phase === 15 ) {
    // you can only get to this phase from phase 14, meaning the max number of rounds has not been reached.
    // do not put anything else here.
    round_begin = millis();
    phase = 7; // so send back here to begin the next round of trials
  }
  
  // Instructions 5 - end of experiment & show score
  if ( phase === 16 ) {
    
    proceed = true;
    
    displayInstructions5();
    
    score_button.show();
  }
  
  if ( phase === 17 ) {  
  	// everything here executes ONCE

    // calculate total score
    total_score = score_per_round.reduce(getSum); // the total number of test trials they got correct
    total_score_percentage = round(total_score/(number_test_trials*score_per_round.length)*100); // divide by total number of test trials and round it
    // (score_per_round.length = total # of rounds participant did.)
    
    //print(total_score);
    //print(number_test_trials);
    //print(score_per_round.length);
    //print(number_test_trials*score_per_round.length);
    //print(total_score_percentage);

    // send info to server to update the Chain or Individual entity
    finishExperiment();

    phase++;
    
    score_button.hide();
    next_button.hide();
  }
  
  if ( phase === 18 ) {
    displayDebrief();
    
    next_button.hide();
  }

}

// END PHASES
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

function arraysEqual(arr1, arr2) {
  if (arr1.length !== arr2.length) { 
    return(false); 
  }
  for (var i = arr1.length; i--;) {
    if(arr1[i] !== arr2[i]) {
      return(false);
    }
  }
  return true;
}

function getSum(total, num) { // use on an integer array like this: yourarray.reduce(getSum);
  return total + num;
}

function correctsChecker(old_labels,new_labels) {
  sames = 0;
  for (var i = old_labels.length; i--;) {
    if (old_labels[i] === new_labels[i]) {
      sames++;
    }
  }
  return sames;
}

function convergenceChecker(old_labels,new_labels) {
  a = arraysEqual(old_labels,new_labels)
  if ( a === true ) {
    converged = true;
  } else {
    converged = false;
  }
}

function deepCopyArray(array_to_copy) {  // um, I think this does a deep copy...
  new_array = [];
  for (var i=0; i < array_to_copy.length; i++) {
    new_array.push(array_to_copy[i]);
  }
  return(new_array);
}

function ownRange(end) {
  a = Array(end);
  for (var i=0; i < a.length; i++) {
    a[i] = i;
  }
  return(a);
}

function testMapper() { // create correct_labels from testresponses & testset_ID
  //print("remap labels now");
  //print("old label map: "+str(correct_labels));
  //print("object_ID: "+str(object_ID));
  //print("testset_ID: "+str(testset_ID));
  //print("testresponses: "+str(testresponses));
  correct_labels = []; // gonna re-write correct labels here
  for (var i=0; i < object_ID.length; i++) { // go through all of the objects by their ID, in order, from object_ID = 0 to object_ID = max
    index = testset_ID.indexOf(i); // get index of object i in testset_ID (i.e. the testing order of objects)
    // then get the testresponse label used for that object, and append it to the new correct_labels variable (still in order of object_ID, 0 to max):
    correct_labels.push(testresponses[index]);
  }
  //print("new label map: "+str(correct_labels));
}

function randoMapper(type) {
  // create the random initial mapping of words to objects
  correct_labels = []; // just in case it already contains values... but it will not where randoMapper() is gonna be used.
  if ( type === "binomial" ) {
    for ( var i=0; i < object_ID.length; i++ ) { // for each frequency...
      val = int(random(0,2));
      correct_labels.push(val);
    }
  }
  if ( type === "equal" ) {
    for ( var j=0; j < object_ID.length/2; j++ ) { // for each frequency...
      correct_labels.push(0);
      correct_labels.push(1);
    }
  }
  randomizeArray(correct_labels);
  //print("correct labels");
  //print(correct_labels);
}

function chooseLabels() {
  maxl = words0.length;
  wordpair = int(random(0, maxl));
  //print("ChooseLabels() says: and the randomly chosen word pair is..."+str(wordpair));
  word0 = words0[wordpair]; // word0 ALWAYS shown on trial left
  word1 = words1[wordpair]; // word1 ALWAYS shown on trial right
}

function loadData() {

  ///////////////////////////////////////////////////////////////////////
  // get condition-specific variables from Google Datastore

  // must set the following variables for both conditions:
  //
  // entityID
  // c_code
  // frequency_condition
  // chain
  // generation
  // parent
  // correct_labels

  // make a GET request to server.py's joinExperiment handler
    $(document).ready(function(){
      $.get({
        url: "join_experiment",
        success: function(data) { // the GET response arrives in a pre-packaged variable called "data"
          // incoming data from server.py is a string formatted like this:
          // chosen_id;cnd;cod;dis;chn;gen;trn;prn;current_time
          incoming = data.split(";") // split the string into its variables
          //console.log(incoming)
          entityID = incoming[0] // assign each incoming variable to an existing js global variable
          condition = incoming[1]
          c_code = incoming[2]
          frequency_condition = incoming[3]
          chain = incoming[4]
          generation = incoming[5]
          correct_labels = incoming[6]
          parent = incoming[7]
          jointime_webapp2 = incoming[8] // this is the exact time that's saved under the entity's "status"

          // reassign generation if it wasn't "NA" (i.e. condition = C)
          if (generation != "NA") { generation = int(incoming[5])+1; }  // increase it by one to be this participant's own generation
          
          // reassign c_code if it wasn't "NA" (i.e. condition = I)
          if (c_code == "NA") { c_code = "c"+frequency_condition+nf(int(random(999999,100000))); } // generate 6 digits - and format as a string with nf()
          
          // if all of the slots are full, server.py will send an empty string.
          if (incoming == "") {
            //console.log("SNAGGED IT!!!!!!!!!!!")
            alert("All of the slots in this experiment are currently full.  If you're still interested in taking this experiment, please try back in 5 minutes. \n\nIf you get this message more than once, please don't try back - that means the experiment is about to end.  \n\nThank you!")
            // if the user closes the error box, experiment.js will stay looping in phase=-1 until the browser is closed.
          } else {
            callback = true  // callback. when callback = true it means the get request was a success (i.e. data for a slot was returned)
          }
          
        },
        error: function(data) {
          // the new join_experiment handler returns an empty string now, where all slots are taken.  So moving the alert to an if statement above.
          //alert("All of the slots in this experiment are currently full.  If you're still interested in taking this experiment, please try back in an hour.  Thanks!")
          alert("There was an error loading the experiment.  Please refresh your browser window to try again.")
        }
      });
    });

}


// OBSOLETE - delete later
function saveLastRound() {
  // record data from last round
  // rewriting the variables from the last round
  
  // stimuli order - training
  trainset_ID_string = join(str(trainset_ID),",");
  trainorders_string = trainorders_string+trainset_ID_string+";";
  //print(trainorders_string);
  
  // stimuli order - testing
  testset_ID_string = join(str(testset_ID),",");
  testorders_string = testorders_string+testset_ID_string+";";
  //print(testorders_string);
  
  // response order - training
  temp1 = join(str(trainresponses),",");
  trainresponses_string = trainresponses_string+temp1+";";
  //print(trainresponses_string);
  
  // response order - testing
  temp2 = join(str(testresponses),",");
  testresponses_string = testresponses_string+temp2+";";
  //print(testresponses_string);
  
  // response changes - training
  temp3 = join(str(trainresponse_changes),",");
  trainresponse_changes_string = trainresponse_changes_string+temp3+";";
  //print(trainresponse_changes_string);
  
  // response changes - testing
  temp4 = join(str(testresponse_changes),",");
  testresponse_changes_string = testresponse_changes_string+temp4+";";
  //print(testresponse_changes_string);
  
  // RTs - training
  temp5 = join(str(train_RTs),",");
  train_RTs_string = train_RTs_string+temp5+";";
  //print(train_RTs_string);
  
  // RTs - testing
  temp6 = join(str(test_RTs),",");
  test_RTs_string = test_RTs_string+temp6+";";
  //print(test_RTs_string);
  
  // correct_labels
  temp7 = join(str(correct_labels),",");
  correct_labels_string = correct_labels_string+temp7+";";
  //print(correct_labels_string);

}

function myUTC(date_object) {
  // create a string formatted the same as webapp2's UTC time
  h = addInitialZero(date_object.getUTCHours());
  m = addInitialZero(date_object.getUTCMinutes());
  s = addInitialZero(date_object.getUTCSeconds());
  d = addInitialZero(date_object.getUTCDate()); // day of the month
  mon = addInitialZero(date_object.getUTCMonth()+1); // coz January = 0 ... December = 11, right.
  y = date_object.getUTCFullYear();
  y = y.toString();
  y = y.slice(-2); // just get the last 2 numbers of the full year
  tzone = "UTC";

  a = [h,":",m,":",s," ",mon,"/",d,"/",y," ",tzone];
  final = a.join("");
  
  return(final);
}

function addInitialZero(integer) {
  // if a date or time unit is only one number long, add an initial zero
  if ( integer.toString().length == 1 ) {
    n = ["0",integer]
    n = n.join("")
  } else {
    n = integer
  }
  return(n)
}

function sendLastRound() {

  // then send all data to server.py in an ajax POST request
  //console.log("javascript is sending the round data...");

  // calculate durations of some parts of experiment here
  duration_consent = round(consent_millis); // ms between experiment loaded and consent was given
  duration_callback = round(callback_millis-consent_millis); // ms between consent given and callback received
  duration_comprehension = round(comprehension_millis-callback_millis); // ms spent on Instructions1 AND comprehension section
  duration_instruction = round(instruction_millis-comprehension_millis); // ms spent on Instructions2 (that's one single page)
  // duration_round already assigned elsewhere in program flow
  duration_experiment = round(round_end); // ms between experiment load and the current round ended
  whole_experiment = "NA"
  
  // determine if this is the participant's final round or not
  if ( converged === true || round_counter === max_rounds ) { // condition "C" sets max_rounds = 1
    final_round = true
    whole_experiment = round(duration_experiment/60000) // x milliseconds / 60,000 = y minutes
  } else { final_round = false }
  
  // below I want to send 2 variables: "correct_labels" and "produced_labels"
  // but when this function is called, correct_labels has already been re-set to the user's last test responses
  // so I'll make new variables (to avoid confusion) and fill them correctly:
  correct_mapping = last_round_labels // the correct mapping of labels to objects in the current round
  produced_mapping = correct_labels  // the mapping that the participant produced in the testing phase of the current round

  send_me = "c_code="+c_code+"&session_code="+session_code+"&condition="+condition+"&frequency_condition="+frequency_condition+"&chain="+chain+"&generation="+generation+"&word0="+word0+"&word1="+word1+"&round_counter="+round_counter+"&final_round="+final_round+"&converged="+converged+"&correct_mapping="+last_round_labels+"&produced_mapping="+correct_labels+"&score="+score+"&trainset_ID="+trainset_ID+"&testset_ID="+testset_ID+"&trainresponses="+trainresponses+"&testresponses="+testresponses+"&trainresponse_changes="+trainresponse_changes+"&testresponse_changes="+testresponse_changes+"&train_RTs="+train_RTs+"&test_RTs="+test_RTs+"&duration_consent="+duration_consent+"&duration_callback="+duration_callback+"&duration_comprehension="+duration_comprehension+"&duration_instruction="+duration_instruction+"&duration_round="+duration_round+"&duration_experiment="+duration_experiment+"&whole_experiment="+whole_experiment+"&comprehension_tally="+comprehension_tally+"&experiment="+experiment+"&parent="+parent+"&jointime_webapp2="+jointime_webapp2+"&jointime_user="+jointime_user+"&jointime_user_UTC="+jointime_user_UTC;
  //"key="+val+"&key="+val+"&key="+val+"&key="+val+"&key="+val+"&key="+val+"&key="+val+"&key="+val+"&key="+val+"&key="+val;

  $(document).ready(function(){
    $.post({
      url: "save_round_data", // goes to server.py's handler "saveRound" via this relative url: /save_round_data
      data: send_me,
      success: function(data) {
        response = data;
      }
    });
  });

}

function displayTrialBlank() {
  if( millis() > timestamp + trial_blank_time ) {
    
    // if it's train trials now
    if ( feedback === true ) {
      if( train_trial_counter < number_train_trials ) { // then round is not complete yet
        phase++; // --> goes to displayTrial()
        train_trial_counter++; // train trial counter increases on first blank, (and this blank occurs BEFORE the first train trial)
        //print("train trial:"+str(train_trial_counter));
      } else {
        phase = 10; // go to Instruction 3
      }
    }
    
    // if it's test trials now
    if ( feedback === false ) {
      if( test_trial_counter < number_test_trials ) { // then round is not complete yet
        phase++; // --> goes to displayTrial()
        test_trial_counter++; // test trial counter increases on first blank, (and this blank occurs BEFORE the first test trial)
        //print("test trial:"+str(test_trial_counter));
      } else {
        round_end = millis();
        duration_round = round(round_end-round_begin);
        phase = 13; // go to save round then to Instruction 4 //???
      }
    }
    
    // reset these to correctly count the responses changes for next trial
    response_change = 0;
    last_click = "NA";
    
    // reset so reaction time recording works
    RT_obtained = false;
    
    timestamp = millis();
    RT_begin = millis();
    proceed = false;
    
    //remove:
    correct_label = correct_labels[trainset_ID[train_trial_counter-1]];
    //print("correct answer: "+str(correct_label));
  }
}

function displayTrial() {
  
  ////////////////////////////////////////////////////////////////////////
  // excute this section at trial end (each line is executed exactly once)
  if ( test_ok === true ) { // this means the user just clicked the OK button
    test_right = false;
    test_left = false;
    test_ok = false;
    proceed = false; // for the OK button that ends feedback
    
    // if it's train trials now
    if ( feedback === true ) { 
      /////////////////////////// save responses
      trainresponses.push(resp);
      trainresponse_changes.push(response_change);
      train_RTs.push(RT);
      phase++; 
    } // go back to feedback
    
    // if it's test trials now
    if ( feedback === false ) { 
      /////////////////////////// save responses
      testresponses.push(resp);
      testresponse_changes.push(response_change);
      test_RTs.push(RT);
      phase--; 
    }  // go back trial blank

    timestamp = millis();
  }
  ////////////////////////////////////////////////////////////////////////
  
  ////////////////////////////////////////////
  // display stimulus
  
  // color is different and defined by object_ID
  push(); 
  if ( feedback === true ) { // training
    ind = trainset_ID[train_trial_counter-1];
    fill(R[ind],G[ind],B[ind]); 
  }
  if ( feedback === false ) { // testing
    ind = testset_ID[test_trial_counter-1];
    fill(R[ind],G[ind],B[ind]);
    push(); fill(0);
    pop();
  }
  ellipse(width/2,height/2,180,180); 
  pop();
  
  // shell pic is same for all objects
  image(shell_pic,width/2,height/2);  // must go below colored ellipse because this image is a mask
  ////////////////////////////////////////////
  
  // display response buttons
  if( millis() > timestamp + stim_only_time ) { 
    push(); 
    
    textSize(40); textAlign(CENTER,CENTER);
    text(word0,200,500); // word0 is ALWAYS on the left
    text(word1,600,500); // word1 is ALWAYS on the right
    
    rectMode(CENTER,CENTER);
    testButton(right_testbutton_x, right_testbutton_y, testbutton_width, testbutton_height, test_right); // left test button
    testButton(left_testbutton_x, left_testbutton_y, testbutton_width, testbutton_height, test_left);    // right test button
    
    // response stays this value until rewritten by this exact code:
    if ( test_left === true ) { resp = 0; }
    if ( test_right === true ) { resp = 1; }
    
    // prevent the OK button click location from working until a response is selected
    if ( test_right || test_left === true ) {
      testButton(test_ok_x,test_ok_y,test_ok_width,test_ok_height,test_ok); // test OK button
      textSize(20);
      text("OK",width/2,test_ok_y+3);
      proceed = true;
    }
    
    pop();
  }
}

function displayFeedback() {
  
  if ( test_ok === true ) {
    test_ok = false;
    phase = 7; // go back to blank
    timestamp = millis();
  }
  
  push(); 
  ind = trainset_ID[train_trial_counter-1]; // training only, because we're in displayFeedbck() now
  fill(R[ind],G[ind],B[ind]); 
  ellipse(width/2,height/2,180,180); 
  pop();
  
  image(shell_pic,width/2,height/2);
  
  push(); 
  textAlign(CENTER,CENTER); rectMode(CENTER,CENTER); textSize(30);
  
  image(alien2_pic,690,150);
  
  // determine feedback type: right or wrong
  correct_label = correct_labels[trainset_ID[train_trial_counter-1]]; // 0 = word0 = left, 1 = word1 = right
  if ( correct_label === resp ) {
    if ( resp === 0 ) {
      text("That's right!  \n\nIt's a "+str(word0)+".",width/2,100);
    }
    if ( resp === 1 ) {
      text("That's right!  \n\nIt's a "+str(word1)+".",width/2,100);
    }
  } else {
    if ( resp === 0 ) {
      text("That's wrong.  \n\nIt's a "+str(word1)+".",width/2,100);
    }
    if ( resp === 1 ) {
      text("That's wrong.  \n\nIt's a "+str(word0)+".",width/2,100);
    }
  }
  
  //////////////////////////////////////////////////////////////////////////////////////
  // I got rid of the 2nd OK button by changing this section ONLY
  // all other things relating to the 2nd OK were left in the code so you can revert it
  
  // TO REVERT: comment this first section back in - and comment the next section out.

  //if( millis() > timestamp + feedback_time ) {
    //testButton(test_ok_x,test2_ok_y,test_ok_width,test_ok_height,test_ok);
    //textSize(20);
    //text("OK",width/2,test2_ok_y+3);
    //proceed = true;
  //}
  
  if( millis() > timestamp + feedback_time ) {
    test_ok = true; // just set test_ok to true as if the button were pushed
    proceed = true;
  }
  //////////////////////////////////////////////////////////////////////////////////////
  
  pop();
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// training and testing set order

function buildTrainset() {
  trainset_ID = []; // clear all entries from previous round
  for (var i=0; i < object_ID.length; i++) {
    for ( var j=0; j < object_frequency[i]; j++ ) { // for each frequency...
      trainset_ID.push(object_ID[i]); // copy the object ID that many times
    }
  }
  randomizeArray(trainset_ID);
}

// one test trial per stimulus
function buildTestset() {
  //testset_ID = object_ID;  // ERROR: this randomized object_ID because it was a "shallow copy": it made both variables refer to the same thing
  testset_ID = ownRange(object_ID.length); // so create testset_ID anew
  randomizeArray(testset_ID);
}


////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// instruction pages and forms

function displayConsent() {
  image(notebook_pic,width/2,height/2);
  
  push(); textStyle(ITALIC); textSize(30);
  text("Informed Consent for Experimental Participants",42,100); pop();

  text("Researchers:",42,150);  
  text("Vanessa Ferdinand, Charles Kemp, Amy Perfors",200,150); 
  text("Affiliation:",42,180);
  text("Melbourne School of Psychological Sciences",200,180);

  text("The consent form is located on the HIT page.  Please read it carefully.\
  \n\nIf you have any questions, please email us at compcogsciadelaide@gmail.com",42,270);

  text("Tick these boxes to affirm that:",42,450);
  
  text("I have read and understood the consent form",100,490);
  text("I am at least 18 years old",100,520);
  text("I agree to participate in this study",100,550);
  
  checkBox(76,482,20,20,check1);
  checkBox(76,512,20,20,check2);
  checkBox(76,542,20,20,check3);
  
  if (check1 && check2 && check3 === true) {
    allChecked = true;
  }
}

function displayInstructions1() {
  //image(alien1_pic,580,360);
  //image(arrow,440,260,100,100);
  image(alien1_pic,580+20,360);
  image(arrow,440+10,260+4,100,100);
  
  push(); textSize(30); text("Instructions",leftmargin,topmargin); pop();
  text("Welcome to planet Zorg!\n\nBiologists on this planet have identified two kinds of sea life.\
  They are two types of shells.  There is only one way to tell these shells apart - it's by their color.",leftmargin,bodymargin,750,600);
  text("\n\n\n\n\nYou will be paired with a biologist from Zorg who will help you learn how to identify the shells.\
  \n\nThe two of you will go on an expedition to the ocean to collect shells and label them.\
  Your goal is to learn how to label the shells correctly.",leftmargin,bodymargin,400,600);
}

function displayInstructions2() {
  push(); textSize(30); text("Instructions",leftmargin,topmargin); pop();
  text("Excellent! You're ready to begin the expedition.\n\nEach shell you find will appear in the middle of the screen.\
  Two possible labels for the shells will appear at the bottom of the screen.\
  Look at the shell and then guess what it is by clicking on one of the labels.\
  Then, click the \"OK\" button when you are done and the biologist will tell you whether you were right or wrong.",leftmargin,bodymargin,750,600);
  push(); textStyle(ITALIC);
  
  if ( condition == "C" ) {
    text("You have been assigned to the short version of this experiment. \nIt will take about 5 minutes to complete.\
    \n\nRemember, please do not write anything down.\
    \nWe are interested in how people naturally learn to label the shells in this task.",leftmargin,300,750,600);
  }
 
  if ( condition == "I" ) {
    text("You have been assigned to longer version of this experiment. \nIt will take about 15 to 30 minutes to complete.\
    \nWe will bonus you based on the length of the experiment at $10 per hour.\
    \n\nRemember, please do not write anything down.\
    \nWe are interested in how people naturally learn to label the shells in this task.",leftmargin,300,750,600);
  }
  pop();

}

//333
function displayInstructions3() {
  proceed = true;
  image(alien3_pic,690,150);
  
  push(); 
  textAlign(CENTER);
  if ( round_counter === 1 ) {
    text("There's an emergency at base camp, I'll be right back. \n\nPlease keep working while I'm away!",-74,80,750,600);
  } else {
    text("There's another emergency at base camp, I'll be right back. \n\nPlease keep working while I'm away!",-74,80,750,600);
  }
  pop();
  
  push();
  textAlign(LEFT); textStyle(ITALIC);
  text("Now the biologist will not be here to tell you if you're right or wrong.\
  \n\nWhen you find a shell, click on the correct label. \nPlease try your best, even though the biologist is gone.\
  \n\nLater, the biologist will look at your work and tell you how many of these you got right.",leftmargin,280,750,600);
  pop();

}

//444
function displayInstructions4() {
  proceed = true;
  
  image(alien4_pic,690,150);
  
  push(); 
  textAlign(CENTER);
  text("Hey, I'm back! \n\nNice job on labelling those shells without me.\
  \n\n I checked your work and you got "+str(score)+" out of 10 right.\
  \n\n\n\nLet's do some more together!",-74,80,750,600);
  pop();
}

//555
function displayInstructions5() {
  image(alien4_pic,690,150);
  
  push(); 
  textAlign(CENTER);
  text("Hey, I'm back! \n\nNice job on labelling those shells without me.\
  \n\n I checked your work and you got "+str(score)+" out of 10 right.\
  \n\n\n\nWell, we're all done now!\
  \n\nYou were an excellent assistant.  Thank you!\
  \n\n Click the button below to get your completion code.",-74,80,750,600);
  pop();
}


function displayComprehension() {
  push(); textSize(30); text("Comprehension questions",leftmargin,topmargin); pop();
  push(); textStyle(ITALIC); text("You must answer these questions correctly to proceed to the experiment.\
  You have \nthree tries to get them all right.  Click \"Back\" if you want to see the instructions again.",leftmargin,topmargin+40); pop();
  push(); stroke("grey"); v = 70+v_adjust; line(leftmargin,v,width-leftmargin,v); pop();
  text("How do the shells differ?",leftmargin,bodymargin+v_adjust);
  push(); stroke("grey"); v = 70+120+v_adjust; line(leftmargin,v,width-leftmargin,v); pop();
  text("Who is the alien?",leftmargin,bodymargin+120+v_adjust);
  push(); stroke("grey"); v = 70+240+v_adjust; line(leftmargin,v,width-leftmargin,v); pop();
  text("Why is the alien there with you?",leftmargin,bodymargin+240+v_adjust);
  push(); stroke("grey"); v = 70+360+v_adjust; line(leftmargin,v,width-leftmargin,v); pop();
  
  // radio button answer order is not randomized, it's hard coded.
  answer1 = radio1.value(); // correct = 4
  answer2 = radio2.value(); // correct = 2
  answer3 = radio3.value(); // correct = 3
  
  if ( comprehension_warning === true ) {
    push(); textAlign(CENTER,CENTER); fill("red");
    if ( comprehension_tally < tally_limit-1) {
      text("One or more of your answers is wrong.\nGo back to read the instructions and try again.",width/2,506+26);
    }
    if ( comprehension_tally === tally_limit-1) {
      text("One or more of your answers is still wrong.\nYou only have once chance left to get them right!",width/2,506+26);
    }
    pop();
  }
}

function displayBootout() {
  textAlign(LEFT);
  text("We see that you have submitted incorrect answers to the comprehension test too many times.\
  Unfortunately, you will not be able to proceed with the experiment.",leftmargin,topmargin,750,600);
  //\n\nPlease copy and paste this code into the \"completion code\" box on the HIT page:\
  //\n\n\n\n\n\nWe will give you a partial reimbursement for the work you did so far.\
  //\n\nWe really appreciate the time you took to help us with our study!",leftmargin,topmargin,750,600);

  //bootout_code.position(330,140);
  //bootout_code.style('color','#266DC6');
  //bootout_code.style('font-size: 40px');
  //bootout_code.show();
}


function displayDebrief() {
  
  if ( frequency_condition === "U" ) {
    
    push(); fill(255); stroke("#2b64bf"); strokeWeight(8); rect(30,170,740,400); pop();
    
    completion_code.position(30,78);
    completion_code.style('color','#2b64bf');
    completion_code.style('font-size: 30px');
    completion_code.show();
    
    //text("You got "+str(total_score_percentage)+"% of the shell labels correct.  Good job! ",30,50);
    
    text("Please copy and paste this completion code onto the \nHIT page so we know you have done the experiment:",30,50);
    
    push(); textStyle(ITALIC); textSize(30); text("What was this experiment about?",50,210); pop();
    text("This experiment is investigating how people learn to categorize things and how the categories they learn can change over time. First, we want to know if showing some objects more often than other objects affects where people put their category boundaries. In the experiment you took, you saw objects with equal frequencies. However, other participants saw all of the objects with highly unequal frequencies. We are going to compare everyone's behavior to see if the frequency differences affect the category boundaries and how.",50,260,710);
    push(); textStyle(BOLD); text("Thank you for taking our experiment. We hope you had fun doing it!",50,450); pop();
    push(); textSize(16);
    text("This research has been cleared by the Human Research Ethics Committee (HREC 1953576.1). \
    \nIf you have any concerns about this project please contact the Executive Officer, Human Research \
    \nEthics, The University of Melbourne (Tel: +61 383 442 073; Fax: +61 393 476 739).",50,500);
    pop();
    
  } else { 
  
    push(); fill(255); stroke("#2b64bf"); strokeWeight(8); rect(30,170,740,400); pop();
    
    completion_code.position(30,78);
    completion_code.style('color','#2b64bf');
    completion_code.style('font-size: 30px');
    completion_code.show();
    
    //text("You got "+str(total_score_percentage)+"% of the shell labels correct.  Good job! ",30,50);
    
    text("Please copy and paste this completion code onto the \nHIT page so we know you have done the experiment:",30,50);
    
    push(); textStyle(ITALIC); textSize(30); text("What was this experiment about?",50,210); pop();
    text("This experiment is investigating how people learn to categorize things and how the categories they learn can change over time. First, we want to know if showing some objects more often than other objects affects where people put their category boundaries. In the experiment you took, you saw objects with highly unequal frequencies. However, other participants saw all of the objects with equal frequencies. We are going to compare everyone's behavior to see if the frequency differences affect the category boundaries and how.",50,260,710);
    push(); textStyle(BOLD); text("Thank you for taking our experiment. We hope you had fun doing it!",50,450); pop();
    push(); textSize(16);
    text("This research has been cleared by the Human Research Ethics Committee (HREC 1953576.1). \
    \nIf you have any concerns about this project please contact the Executive Officer, Human Research \
    \nEthics, The University of Melbourne (Tel: +61 383 442 073; Fax: +61 393 476 739).",50,500);
    pop();
    
  }

}

function finishExperiment() {  // do this in phase 17
	
	// I: sets status to "DONE"
	// C: sets status to "available"

	if ( condition === "C" ) {

    send_me = "chainID="+entityID+"&c_code="+c_code+"&transmit="+produced_mapping+"&previous="+correct_mapping+"&generation="+generation;

    $(document).ready(function(){
      $.post({
        url: "leave_chain", // goes to server.py's handler "leaveChain" via this relative url: /leave_chain
        data: send_me,
        success: function(data) {
          response = data; // send some info to server.py
        }
      });
    });
  }

  if ( condition === "I" ) {

    send_me = "individualID="+entityID;

    $(document).ready(function(){
      $.post({
        url: "leave_individual", // goes to server.py's handler "leaveIndividual" via this relative url: /leave_individual
        data: send_me,
        success: function(data) {
          response = data; // send some info to server.py
        }
      });
    });
  }

}

function abortExperiment() {  // call this when participant reloads or closes the browser
	
  // I: sets status to "available"
  // C: sets status to "available"

  if ( condition === "C" ) {

    send_me = "chainID="+entityID+"&experiment="+experiment;

    $(document).ready(function(){
      $.post({
        url: "abort_chain", // goes to server.py's handler "leaveChain" via this relative url: /leave_chain
        data: send_me,
        success: function(data) {
          response = data; // send some info to server.py
        }
      });
    });
  }

  if ( condition === "I" ) {

    send_me = "individualID="+entityID+"&experiment="+experiment;

    $(document).ready(function(){
      $.post({
        url: "abort_individual", // goes to server.py's handler "leaveIndividual" via this relative url: /leave_individual
        data: send_me,
        success: function(data) {
          response = data; // send some info to server.py
        }
      });
    });
  }

}

function sendVitals() {
  // setInterval starts calling this function in setup
  // but it can only send data once all of these variables to send are defined
  // in phase 1, all variables below are guaranteed to be defined
  // so wait until phase > 1 before sendVitals() starts making the POST requests
  if (phase > 1) {
    
    send_me = "entityID="+entityID+"&session_code="+session_code+"&condition="+condition+"&frequency_condition="+frequency_condition+"&phase="+phase;

    $(document).ready(function(){
      $.post({
        url: "send_vitals", // goes to server.py's handler "leaveIndividual" via this relative url: /leave_individual
        data: send_me,
        success: function(data) {
          response = data; // send some info to server.py
        }
      });
    });
    
    //console.log("ping")
    
  }

}


function overButton(x, y, w, h) {
  if (mouseX >= x-(w/2) && mouseX <= x+(w/2) && mouseY >= y-(h/2) && mouseY <= y+(h/2)) { return true; } 
  else { return false; }
}

function checkBox(x, y, w, h, check_boolean) {
  push(); fill(255); stroke(0); strokeWeight(1); rectMode(CENTER);
  rect(x,y,w,h);
  if ( overButton(x,y,w,h) === true ) {
    fill(0);
    rect(x,y,w,h);
  }
  if ( check_boolean === true ) {
    fill(0);
    rect(x, y, w, h);
  }
  pop();
}

function testButton(x, y, box_width, box_height, test_boolean) {
  push();
  if ( overButton(x, y, box_width, box_height) === true ) { 
    noFill();
    stroke(0);
    //stroke(150);
    strokeWeight(3);
    rect(x, y, box_width, box_height);
  }
  if ( test_boolean === true ) {
    noFill();
    stroke(0);
    //stroke(150);
    strokeWeight(7);
    rect(x, y, box_width, box_height);
  }
  pop();
}

function randomizeArray(old) { // code for randomizing an array, courtesy of Jelle Zuidema
  var candidate, oldval;
  for (var j=0; j<old.length; j++) {
    candidate = int(j + random(old.length-j));
    oldval=old[j]; old[j]=old[candidate]; old[candidate]=oldval;
  }
}


function mousePressed() {
  if ( phase === -3 ) { // CONSENT REVERT 2 of 2 - change this back to phase === 1
    if ( overButton(76,482,20,20) === true ) { check1 = true; }
    if ( overButton(76,512,20,20) === true ) { check2 = true; }
    if ( overButton(76,542,20,20) === true ) { check3 = true; }
  }
  if ( phase === 8 ) {  // train trials
    if ( overButton(right_testbutton_x, right_testbutton_y, testbutton_width, testbutton_height) === true ) { 
      if ( last_click === "L" ) { response_change++; }
      last_click = "R";
      if ( RT_obtained === false ) { // meaning this is the first click on a response button
        RT_end = millis();
        //print(RT_end,30,30);
        RT_obtained = true;
        RT = round(RT_end-RT_begin);
      }
      test_right = true; 
      test_left = false; 
    }
    if ( overButton(left_testbutton_x, left_testbutton_y, testbutton_width, testbutton_height) === true ) { 
      if ( last_click === "R" ) { response_change++; }
      last_click = "L";
      if ( RT_obtained === false ) { // meaning this is the first click on a response button
        RT_end = millis();
        //print(RT_end,30,30);
        RT_obtained = true;
        RT = round(RT_end-RT_begin);
      }
      test_left = true; 
      test_right = false; 
    }
    if ( proceed === true ) {
      if ( overButton(test_ok_x, test_ok_y, test_ok_width, test_ok_height) === true ) { test_ok = true; }
    }
  }
  if ( phase === 9 ) {  // feedback
    if ( proceed === true ) {
      if ( overButton(test_ok_x, test2_ok_y, test_ok_width, test_ok_height) === true ) { test_ok = true; }
    }
  }
  if ( phase === 12 ) {  // test trials
    if ( overButton(right_testbutton_x, right_testbutton_y, testbutton_width, testbutton_height) === true ) { 
      if ( last_click === "L" ) { response_change++; }
      last_click = "R";
      if ( RT_obtained === false ) { // meaning this is the first click on a response button
        RT_end = millis();
        RT_obtained = true;
        RT = round(RT_end-RT_begin);
      }
      test_right = true; 
      test_left = false; 
    }
    if ( overButton(left_testbutton_x, left_testbutton_y, testbutton_width, testbutton_height) === true ) { 
      if ( last_click === "R" ) { response_change++; }
      last_click = "L";
      if ( RT_obtained === false ) { // meaning this is the first click on a response button
        RT_end = millis();
        RT_obtained = true;
        RT = round(RT_end-RT_begin);
      }
      test_left = true; 
      test_right = false; 
    }
    if ( proceed === true ) {
      if ( overButton(test_ok_x, test_ok_y, test_ok_width, test_ok_height) === true ) { test_ok = true; }
    }
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// html button navigators

function next_phase() {
  
  ///////////////////////////////////////////////////////
  // per-phase ways to proceed while "proceed === false":
  
  if ( phase === 5 ) {
    if ( answer1 === "4" && answer2 === "2" && answer3 === "3" ) { // all answers are currently set to the correct ones
      proceed = true; // then it executes the general phase++ code below
      //noLoop(); timestamp = millis(); phase++; loop();
    } else {
      comprehension_warning = true; // makes the warning message come on screen
      comprehension_tally++;
    }
  }
  
  ///////////////////////////////////////////////////////
  // the general way to proceed (happens only when proceed === true)
  
  if ( proceed === true ) { noLoop(); timestamp = millis(); phase++; loop(); }
  // proceed gets set to false when a question is unanswered / answered incorrectly
  
  // button click first changes the phase in line above, then then executes whatever's in "if phase x (new phase #) do y"

}

function prev_phase() {
  noLoop(); timestamp = millis(); phase--; loop();
}
