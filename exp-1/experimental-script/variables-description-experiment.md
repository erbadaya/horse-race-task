This is the 'code' for Exp. 1 of the Horse Race Experiment (see Pre-reg document), as implemented on Testable.

# Experiment flow

Participants first report their Prolific ID, and answer 3 pre-screening questions (which were also set as filters on Prolific): be born and raised in the UK, currently residing in the UK, and be English monolingual. If participants fail to meet these three criteria, they are taken out of the experiment and asked to return their submission on Prolific. As the experiment requires listening to auditory stimuli, we included an attention check. Participants listen to auditory instructions asking them to click on a button. If they fail to do so, they are taken out of the experiment and asked to return their submission on Prolific. If participants meet all criteria and follow the auditory instructions, the experiment begins.

They are first shown two instruction screens. First, they are introduced to 'two well-renowed tipsters'.

> We have received information from the UK and Italy's leading horse racing tipsters, Lauren Waterstone and Marila Fiammetta, about the upcoming 'Start of the season' race on Tuesday 2nd April at Musselburgh Racecourse (Edinburgh). <br> Ms Waterstone and Ms Fiammetta are highly respected tipsters amongst the horse racing elites in the UK and Italy. Both have achieved an outstanding level of performance that marks them as the very best in the game. They currently stand undefeated, having had an unbeatable strike of consecutive wins since 2021. <br>
We are truly honoured to have received information from Ms Waterstone and Ms Fiammetta on the 4 most popular horses running at Musselburgh in April. <br>
Click 'NEXT' to continue.

Afterwards, they are shown the four horses that will participate in said horserace, and are re-explained the instructions.

> This is your betting slip. You can see the name and a picture of the four racehorses. <br> In the experiment, you will hear four passages; one about each horse. You will only hear each passage once. <br> <b>You must place a bet on each horse based on the likelihood you think that horse has of winning.</b> You have two-hundred pounds to split between the four horses. You may split it however you like, and you do not have to spend it all. <br> You can place your bets as you go. You may also choose to modify your bets after listening to all of the passages. There will be further instructions on the screen, which should make your task clear.  <br> Click 'NEXT' to continue.

Once they click 'NEXT' the experiment begins. Participants are randomly allocated to one of 24 lists (Latin Square design, so all horses are described by all speakers in both manners of delivery, but each participant only sees one combination of speaker - manner of delivery; horse pictures and descriptions are fixed). Each list consists of four trials (one per horse), where participants listen twice to the same speaker describing a different horse, one time fluently and one time disfluently. Horse presentation is randomised within lists. 

Each trial has the following logic.
* Preview window (i.e., only see the picture of the horse) for 600 ms.
* Horse description.
* Allocate bet.
* Move onto the next horse.

Once a participant has seen all four horses, they are shown all their bets alongisde the horse pictures. They are allowed to change their bets one last time, as long as they do not bet more than two-hundred pounds.

After participants have allocated their bets, there are a series of questions divided in six sets.

* The **first set** are attention check questions. We ask participants to report to how many speakers they listened to (correct answer: 2), as well as report the speakers' nationalities (Italian and British, for the non-native and native speaker conditions, respectively). If participants fail this attention check, they are not included in our analysis.

* The **second and third sets** form part of a language attitude questionnaire, from Foucart and Hartsuiker (2021). Each set related to each speaker (i.e., non-native/native). These are six questions to measure affect, five questions to measure status, five questions to measure solidarity, one question to measure comprehension, one to measure fluency, and one to measure perceived accentedness. We additionally included a question on perceived trustworthiness, to control for any potential effect on betting behaviour not attributable to manner of delivery. All these questions are on a 0 - 100 scale. Each dimensions (affect/status/solidarity) was presented in separate screens, and order of presentation within each screen, and of the screens was randomised.

* The **forth set** measures participants' exposure to native and foreign-accented English.

* The **fifth set** measures participants' previous experience with horseraces, in terms of betting (yes/no answer) and their knowledge (5-point scale on how they agree with the statement 'I am an expert in horse races', ranging from 'Strongly disagree' to 'Strongly agree'). Participants who report high knowledge (from 'Neither agree nor disagree' to 'Strongly agree') will be discarded from analysis.

* The **sixth set** are two open-questions and a manipulation check. The first question checks whether participants' guessed the actual aim of the experiment. The second question allows participants to communicate with the experimenter if they had any other thoughts/remarks. The third and forth questions refer to the naturalness (defined as 'How likely it is that the recordings you heard were recorded in one go) of the audio, to ensure that our participants did not notice the edits of the fluent recordings.

After this set of questions, data is saved. Participants see a screen debriefing them, and are re-directed to Prolific for reimbursement.

# Variables description

In Testable, each row represents a trial (i.e., presentation of stimuli); because of this logic, in this experiment experimental trials take more than one row. For a full explanation of how Testable works, please visit their manual (e.g., what does presTime mean).

Customised variables in this experiment:

* subjectGroup: Participant's list allocated.
* condition1 (form/critical): Stores whether it was a experimental trial, or a survey trial.
* condition2 (apocalypse/silversky/firewalker/blackblade): Applies only to experimental trials. Stores what horse was described.
* condition3 (native/nonnative): Applies to both experimental trials and sets 2 and 3 of post-experimental questionnaire. Stores whether the audio was a native or a non-native speaker, or whether the questions were about the native or the non-native speaker.
* condition4 (fluent/disfluent): Applies only to experimental trials. Stores whether the audio was delivered fluently or disfluently.
* condition5: Can store several values, depending on whether the trial was a experimental trial, survey element, or just presentation of instructions. This variable stores information about what participants were shown/what were they asked.
    * In *experimental trials* (i.e., condition1 == critical), it can take the values preview/audio/bet/move-on/final-bel. Stores what part of the trial took place in each row.
        * preview: Preview window, appears four times (one per horse).
        * audio: Play recording, appears four times (one per horse).
        * bet: Participants' allocation of money, appears four times (one per horse).
        * move-on: Once participants have placed a bet on a horse so they can progress to the next window, appears four times (one per horse).
        * final-bet: Show participants all their bets and allow them to re-distribute the money, appears one time (at the end of the four experimentla trials).
    * In *survey elements* (i.e., condition1 == form), it can take several values.
        * id, uk_born, uk_residency_engl1, other_l2: First questions in the experiment to check that participants meet our criteria.
        * attention_check: Trials that are experimental checks.
        * affect[1-6], status[1-5], solidarity[1-5], comprehension, fluency, accent, trusthworthy: Stores the question that participants were asked regarding language attitudes (i.e., sets 2 and 3 of post-experimental questionnaire).
        * exposure-nn, exposure-n: Stores set 4 of post-experimental questionnaire.
        * bet-experience-binary, bet-knowledge: Stores set 5 of post-experimental questionnaire.
        * open-question-aim, open-question-thoughts, naturalness: Stores set 6 of post-experimental questionnaire.
    * In *presentation of instructions*, this column is intentionally left blank.
* label: Associated with trials when participants allocate money (i.e., condition5 == bet), consists of the name of the horse being presented and list number. We use this value to present them at the final bet (i.e., condition5 == final-bet). 


