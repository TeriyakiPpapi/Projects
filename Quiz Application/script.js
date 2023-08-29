// BISIWROTE

// Define an array of quiz questions 
const quizQuestions = [
    {
        question: "What is the capital of Lagos?",
        options: [ "Ojodu", "Surelere", "Ikeja", "Abuja"],
        correctAnswer: "Ikeja"
    },
    {
        questions: "Which country has the best Jollof Rice?",
        options: [ "Ghana", "Senagal", "Nigeria", "Ivory Coast"],
        correctAnswer:"Nigeria"
    },
    {
        question : "What is my favorite color ?",
        options: [ "Blue", "Gold", "Brown", "Green"],
        correctAnswer :"Brown"
    }
];

// Variables to track quiz state
let currentQuestionIndex = 0; 
let score = 0; 
let timeRem = 30;
let timerInterval; 

// Function to start the quiz 
function startQuiz() {
    // Hide the start button and display the first question
    documemnt.getElementById("start-button").style.display = "none";
    displayQuestion();
    startTimer();
}

// Function to display a question and its options
function displayQuestion() {
    const currentQuestion = quizQuestions[currentQuestionIndex];
    const questionText = document.getElementById("question-text");
    const answerButtons = document.getElementById("answer-buttons");

    // Clear previous quesitons and answer options 
    questionText.innerHTML = "";
    answerButtons.innerHTML = "";

    // Display the current question
    questionText.innerHTML = currentQuestion.question; 

    // Create answer buttons for each option
    currentQuestion.options.forEach(option => {
        const button = document.createElement("button");
        button.innerText = option;
        button.classList.add("answer-button");
        answerButtons.appendChild(button);

        // Add click event listener to check answer
        button.addEventListener("click", function() {
            checkAnswer(option);
        });
    });
}

// Check if user's selected answer is right or wrong, then move on to next question/end game
function checkAnswer(selectedOption) {
    const currentQuestion = quizQuestions[currentQuestionIndex];

    if (selectedOption === currentQuestion.correctAnswer) {
        score++;
    }
    // move on
    currentQuestionIndex++;

    if (currentQuestionIndex < quizQuestions.length) {
        displayQuestion();
    } else {
        endQuiz();
    }
 }

// Function to start the timer
function startTimer() {
    timerInterval = setInterval(function() {
        timeRem--;

        // Update the timer text 
        document.getElementById("timer").textContent = timeRem;

        // End quiz when time runs out 
        if (timeRem <= 0) {
            endQuiz();
        }
    }, 1000);
}

// function to end quiz
function endQuiz() {
    // stop the timer
    clearInterval(timerInterval);

    // Calculate the score percentage
    const scorePercentage = (score / quizQuestions) * 100;

    // Display the final score 
    const questionContainer = document.getElementById("question-container");
    questionContainer.innerHTML = `
    <h2>Quiz Completed, Good Job :)!</h2>
    <p>Your Score: ${score} out of ${quizQuestions.length}</p>
    <p>Score Percentage: ${scorePercentage}%</p>
    `;
}

// Add event listener to start the quiz when the start button is clicked 
document.getElementById("start-button").addEventListener("click", startQuiz);
    
