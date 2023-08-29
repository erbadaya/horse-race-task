var jsPsychAudioTextKeyboardResponse = (function (jspsych) {
    'use strict';
  
    const info = {
        name: "audio-textkeyboard-response",
        parameters: {
            /** The audio file to be played. */
            stimulus: {
                type: jspsych.ParameterType.AUDIO,
                pretty_name: "Stimulus",
                default: undefined,
            },
            /** Array containing the key(s) the subject is allowed to press to respond to the stimulus. */
            choices: {
                type: jspsych.ParameterType.KEYS,
                pretty_name: "Choices",
                default: "ALL_KEYS",
            },
            /** Any content here will be displayed below the stimulus. */
            preamble: { 
                type: jsPsych.plugins.parameterType.STRING, 
                pretty_name: 'Preamble', 
                default: null, 
                description: 'HTML formatted string to display at the top of the page above all the questions.' 
            }, 
            button_label: { 
                type: jsPsych.plugins.parameterType.STRING, 
                pretty_name: 'Button label', 
                default:  'Continue', 
                description: 'The text that appears on the button to finish the trial.' 
            }, 
            autocomplete: { 
                type: jsPsych.plugins.parameterType.BOOL, 
                pretty_name: 'Allow autocomplete', 
                default: false, 
                description: "Setting this to true will enable browser auto-complete or auto-fill for the form." 
            } ,
            /** The maximum duration to wait for a response. */
            trial_duration: {
                type: jspsych.ParameterType.INT,
                pretty_name: "Trial duration",
                default: null,
            },
            /** If true, the trial will end when user makes a response. */
            response_ends_trial: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Response ends trial",
                default: true,
            },
            /** If true, then the trial will end as soon as the audio file finishes playing. */
            trial_ends_after_audio: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Trial ends after audio",
                default: false,
            },
            /** If true, then responses are allowed while the audio is playing. If false, then the audio must finish playing before a response is accepted. */
            response_allowed_while_playing: {
                type: jspsych.ParameterType.BOOL,
                pretty_name: "Response allowed while playing",
                default: true,
            },
            questions: { 
                type: jsPsych.plugins.parameterType.COMPLEX, 
                array: true, 
                pretty_name: 'Questions', 
                default: undefined, 
                nested: { 
                  prompt: { 
                    type: jsPsych.plugins.parameterType.STRING, 
                    pretty_name: 'Prompt', 
                    default: undefined, 
                    description: 'Prompt for the subject to response' 
                  }, 
                  placeholder: { 
                    type: jsPsych.plugins.parameterType.STRING, 
                    pretty_name: 'Placeholder', 
                    default: "", 
                    description: 'Placeholder text in the textfield.' 
                  }, 
                  rows: { 
                    type: jsPsych.plugins.parameterType.INT, 
                    pretty_name: 'Rows', 
                    default: 1, 
                    description: 'The number of rows for the response text box.' 
                  }, 
                  columns: { 
                    type: jsPsych.plugins.parameterType.INT, 
                    pretty_name: 'Columns', 
                    default: 40, 
                    description: 'The number of columns for the response text box.' 
                  }, 
                  required: { 
                    type: jsPsych.plugins.parameterType.BOOL, 
                    pretty_name: 'Required', 
                    default: false, 
                    description: 'Require a response' 
                  }, 
                  name: { 
                    type: jsPsych.plugins.parameterType.STRING, 
                    pretty_name: 'Question Name', 
                    default: '', 
                    description: 'Controls the name of data values associated with this question' 
                  } 
                } 
            }, 
        },
    };
    /**
     * **audio-keyboard-response**
     *
     * jsPsych plugin for playing an audio file and getting a keyboard response
     *
     * @author Josh de Leeuw
     * @see {@link https://www.jspsych.org/plugins/jspsych-audio-keyboard-response/ audio-keyboard-response plugin documentation on jspsych.org}
     */
    class AudioKeyboardResponsePlugin {
        constructor(jsPsych) {
            this.jsPsych = jsPsych;
        }
        trial(display_element, trial, on_load) {
            // hold the .resolve() function from the Promise that ends the trial
            let trial_complete;
            // setup stimulus
            var context = this.jsPsych.pluginAPI.audioContext();
            // store response
            var response = {
                rt: null,
                key: null,
            };
            // record webaudio context start time
            var startTime;
            // load audio file
            this.jsPsych.pluginAPI
                .getAudioBuffer(trial.stimulus)
                .then((buffer) => {
                if (context !== null) {
                    this.audio = context.createBufferSource();
                    this.audio.buffer = buffer;
                    this.audio.connect(context.destination);
                }
                else {
                    this.audio = buffer;
                    this.audio.currentTime = 0;
                }
                setupTrial();
            })
                .catch((err) => {
                console.error(`Failed to load audio file "${trial.stimulus}". Try checking the file path. We recommend using the preload plugin to load audio files.`);
                console.error(err);
            });
            const setupTrial = () => {
                // set up end event if trial needs it
                if (trial.trial_ends_after_audio) {
                    this.audio.addEventListener("ended", end_trial);
                }
                // show prompt if there is one
                if (trial.prompt !== null) {
                    display_element.innerHTML = trial.prompt;
                }
                // set question defaults
                for (var i = 0; i < trial.questions.length; i++) { 
                    if (typeof trial.questions[i].rows == 'undefined') { 
                    trial.questions[i].rows = 1; 
                    } 
                } 
                for (var i = 0; i < trial.questions.length; i++) { 
                    if (typeof trial.questions[i].columns == 'undefined') { 
                    trial.questions[i].columns = 40; 
                    } 
                } 
                for (var i = 0; i < trial.questions.length; i++) { 
                    if (typeof trial.questions[i].value == 'undefined') { 
                    trial.questions[i].value = ""; 
                    } 
                } 
                // create HTML string variable
                
                if (trial.autocomplete) { 
                    html += '<form id="jspsych-survey-text-form">'; 
                } else { 
                    html += '<form id="jspsych-survey-text-form" autocomplete="off">'; 
                } 
                // generate question order 
                var question_order = []; 
                for(var i=0; i<trial.questions.length; i++){ 
                    question_order.push(i); 
                } 
                if(trial.randomize_question_order){ 
                    question_order = jsPsych.randomization.shuffle(question_order); 
                } 
                
                // add questions 
                for (var i = 0; i < trial.questions.length; i++) { 
                    var question = trial.questions[question_order[i]]; 
                    var question_index = question_order[i]; 
                    html += '<div id="jspsych-survey-text-'+question_index+'" class="jspsych-survey-text-question" style="margin: 2em 0em;">'; 
                    html += '<p class="jspsych-survey-text">' + question.prompt + '</p>'; 
                    var autofocus = i == 0 ? "autofocus" : ""; 
                    var req = question.required ? "required" : ""; 
                if(question.rows == 1){ 
                    html += '<input type="text" id="input-'+question_index+'"  name="#jspsych-survey-text-response-' + question_index + '" data-name="'+question.name+'" size="'+question.columns+'" '+autofocus+' '+req+' placeholder="'+question.placeholder+'"></input>'; 
                } else { 
                    html += '<textarea id="input-'+question_index+'" name="#jspsych-survey-text-response-' + question_index + '" data-name="'+question.name+'" cols="' + question.columns + '" rows="' + question.rows + '" '+autofocus+' '+req+' placeholder="'+question.placeholder+'"></textarea>'; 
                    } 
                    html += '</div>'; 
                } 
                
                // add submit button 
                html += '<input type="submit" id="jspsych-survey-text-next" class="jspsych-btn jspsych-survey-text" value="'+trial.button_label+'"></input>'; 
                
                html += '</form>' 

                display_element.querySelector('#input-'+question_order[0]).focus(); 
                // start audio
                if (context !== null) {
                    startTime = context.currentTime;
                    this.audio.start(startTime);
                }
                else {
                    this.audio.play();
                }
                // start keyboard listener when trial starts or sound ends
                if (trial.response_allowed_while_playing) {
                    setup_keyboard_listener();
                }
                else if (!trial.trial_ends_after_audio) {
                    this.audio.addEventListener("ended", setup_keyboard_listener);
                }
                // end trial if time limit is set
                if (trial.trial_duration !== null) {
                    this.jsPsych.pluginAPI.setTimeout(() => {
                        end_trial();
                    }, trial.trial_duration);
                }
                on_load();
            };
            display_element.querySelector('#jspsych-survey-text-form').addEventListener('submit', function(e) { 
                e.preventDefault(); 
                // measure response time 
                var endTime = performance.now(); 
                var response_time = endTime - startTime; 
             
                // create object to hold responses 
                var question_data = {}; 
                 
                for(var index=0; index < trial.questions.length; index++){ 
                  var id = "Q" + index; 
                  var q_element = document.querySelector('#jspsych-survey-text-'+index).querySelector('textarea, input');  
                  var val = q_element.value; 
                  var name = q_element.attributes['data-name'].value; 
                  if(name == ''){ 
                    name = id; 
                  }         
                  var obje = {}; 
                  obje[name] = val; 
                  Object.assign(question_data, obje); 
                } 
                // save data 
                var trialdata = { 
                  rt: response_time, 
                  response: question_data 
                }; 
             
                display_element.innerHTML = ''; 
             
                // next trial 
                jsPsych.finishTrial(trialdata); 
              }); 
             
              var startTime = performance.now(); 
            }; 
            return new Promise((resolve) => {
                trial_complete = resolve;
            });
        }
        simulate(trial, simulation_mode, simulation_options, load_callback) {
            if (simulation_mode == "data-only") {
                load_callback();
                this.simulate_data_only(trial, simulation_options);
            }
            if (simulation_mode == "visual") {
                this.simulate_visual(trial, simulation_options, load_callback);
            }
        }
        simulate_data_only(trial, simulation_options) {
            const data = this.create_simulation_data(trial, simulation_options);
            this.jsPsych.finishTrial(data);
        }
        simulate_visual(trial, simulation_options, load_callback) {
            const data = this.create_simulation_data(trial, simulation_options);
            const display_element = this.jsPsych.getDisplayElement();
            const respond = () => {
                if (data.rt !== null) {
                    this.jsPsych.pluginAPI.pressKey(data.response, data.rt);
                }
            };
            this.trial(display_element, trial, () => {
                load_callback();
                if (!trial.response_allowed_while_playing) {
                    this.audio.addEventListener("ended", respond);
                }
                else {
                    respond();
                }
            });
        }
        create_simulation_data(trial, simulation_options) {
            const default_data = {
                stimulus: trial.stimulus,
                rt: this.jsPsych.randomization.sampleExGaussian(500, 50, 1 / 150, true),
                response: this.jsPsych.pluginAPI.getValidKey(trial.choices),
            };
            const data = this.jsPsych.pluginAPI.mergeSimulationData(default_data, simulation_options);
            this.jsPsych.pluginAPI.ensureSimulationDataConsistency(trial, data);
            return data;
        }
    }
    AudioKeyboardResponsePlugin.info = info;
  
    return AudioKeyboardResponsePlugin;
  
  })(jsPsychModule);
  