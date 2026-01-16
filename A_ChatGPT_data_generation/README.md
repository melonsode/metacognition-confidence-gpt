# ChatGPT Data Generation (Single-Agent Batch)

This directory contains scripts and materials for generating response data from ChatGPT via the API, treating ChatGPT as an experimental agent comparable to a human participant.

Each execution of the single-agent script corresponds to **one agent (one participant)** and produces **one Excel file** containing responses to a fixed set of questions.

---

## File Structure
.  
├─ Questions_with_answer.xlsx  
├─ raw_responses/  
├─ run_chatgpt_single_agent.py  
└─ run_chatgpt_agents_batch.sh  

## Environment Setup

Before running any scripts, you must set your OpenAI API key as an environment variable.

Run the following command in your shell:

    export OPENAI_API_KEY="sk-***"

Make sure this variable is available in the environment where the scripts are executed.


### Questions_with_answer.xlsx
- Master file containing all questions used in the experiment.
- **Correct answers are predefined at the time of question construction.**
- These answers serve as the *answer key* (ground truth) for later evaluation of ChatGPT responses.
- This file is not modified during data generation.

### raw_responses/
- Directory where response data are stored.
- Each Excel file corresponds to **one ChatGPT agent**, analogous to one human participant.
- Files generated here are treated as **raw response data** and are used for subsequent analysis (e.g., correctness, confidence, correlations).
- **Note:** This directory is currently empty. The response data actually used in the analyses are located in `1_raw_data/ChatGPT_API_n87`.


### run_chatgpt_single_agent.py
- Runs ChatGPT as a **single agent**.
- Before executing this script, you must set the `OPENAI_API_KEY` environment variable (see the Environment Setup section).
- Sequentially sends all questions (e.g., 60 questions) to the ChatGPT API.
- Saves all responses from one agent into **one Excel file** in `raw_responses/`.
- This script represents one experimental run for one agent.


### run_chatgpt_agents_batch.sh
- Batch script that repeatedly executes `run_chatgpt_single_agent.py`.
- Used to generate data for multiple ChatGPT agents (e.g., 87 agents).
- Enables automated, non-interactive data collection.

---

## Notes

- ChatGPT response generation is separated from statistical analysis.
- All statistical analyses are performed downstream using the generated Excel files.
- This design ensures reproducibility and a clear separation between **data generation** and **data analysis**.

---

## Usage (Example)

### Generate data for multiple ChatGPT agents (batch execution)

```sh
sh run_chatgpt_agents_batch.sh 1 87
```

This command generates response data for ChatGPT agents with IDs from 1 to 87 (inclusive).
Each agent corresponds to one experimental run and produces one Excel file in raw_responses/.

### Generate data for a single ChatGPT agent

```sh
python3 run_chatgpt_single_agent.py Questions_with_answer.xlsx 3 7
```

This command runs ChatGPT as a single agent and generates one Excel file containing responses to all questions.


## Command-line Interface

    python3 run_chatgpt_single_agent.py <question_file> <nrow> <agent_id>

## Arguments

- **questions_file**  
  Path to the Excel file containing all questions and their predefined correct answers.  
  This file serves as the master stimulus set presented to ChatGPT.

- **nrow**  
  Number of rows used for a quick preview or verification of the question file during development.  
  This parameter does not affect response generation in batch runs; please set it to `3`.

- **agent_id**  
  Integer identifier for the ChatGPT agent.  
  Corresponds to a participant ID in the human experiment and is used in output file naming.


## Output

- One Excel file is generated in the `raw_responses` directory.  
- Each file corresponds to one ChatGPT agent (one participant).

---

### run_chatgpt_agents_batch.sh

**Usage**

    sh run_chatgpt_agents_batch.sh <start_id> <end_id>

**Arguments**

- **start_id**  
  The first agent ID to generate.

- **end_id**  
  The last agent ID to generate (inclusive).

#### Behavior

- Calls `run_chatgpt_single_agent.py` sequentially for each agent ID in the specified range.
- Generates one Excel file per agent.
- Enables automated batch generation of response data for multiple ChatGPT agents.



