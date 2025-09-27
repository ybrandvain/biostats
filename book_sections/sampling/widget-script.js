document.addEventListener('DOMContentLoaded', function() {
    
    // --- 1. 动态加载问题内容 ---
    const urlParams = new URLSearchParams(window.location.search);
    const promptId = urlParams.get('prompt_id');

    // --- 2. 缓存所有DOM元素 ---
    const widget = document.querySelector('.dv-challenge-widget');
    const widgetTitle = widget.querySelector('#widget-title');
    const questionText = widget.querySelector('#question-text');
    const chartImage = widget.querySelector('#chart-image');
    const answerForm = widget.querySelector('#answer-form');
    const studentAnswerTextarea = widget.querySelector('#student-answer');
    const studentIdInput = widget.querySelector('#student-id');
    const loader = widget.querySelector('.loader');
    const feedbackContainer = widget.querySelector('#feedback-container');
    const feedbackResult = widget.querySelector('#feedback-result');
    const ratingContainer = widget.querySelector('#rating-container');
    const ratingForm = widget.querySelector('#rating-form');
    const ratingThanks = widget.querySelector('#rating-thanks');
    let currentResponseId = null;

    // --- 3. 页面加载时获取问题内容 ---
    if (promptId) {
        const isLocal = window.location.hostname === '127.0.0.1' || window.location.hostname === 'localhost';
        const apiBaseUrl = 'https://ai-stats-book.onrender.com';
        
        fetch(`${apiBaseUrl}/api/question-details/${promptId}`)
            .then(response => response.json())
            .then(data => {
                if (data.error) {
                    widgetTitle.innerText = "Error";
                    questionText.innerText = data.error;
                } else {
                    widgetTitle.innerText = data.title;
                    questionText.innerText = data.question_text;
                    if (data.image_src) {
                        chartImage.src = data.image_src;
                        chartImage.style.display = 'block';
                    }
                }
            });
    }

    // --- 4. 处理学生答案提交的逻辑 ---
    answerForm.addEventListener('submit', function(event) {
        event.preventDefault();
        const studentAnswer = studentAnswerTextarea.value;
        const studentId = studentIdInput.value;
        if (!studentAnswer.trim()) return;

        loader.style.display = 'block';
        feedbackContainer.style.display = 'none';
        ratingContainer.style.display = 'none';
        
       const apiUrl = 'https://ai-stats-book.onrender.com/api/evaluate';

        fetch(apiUrl, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                answer: studentAnswer,
                student_id: studentId,
                prompt_id: promptId
            })
        })
        .then(response => response.json())
        .then(data => {
            loader.style.display = 'none';
            feedbackResult.innerText = data.feedback;
            feedbackContainer.style.display = 'block';
            if (data.response_id) {
                currentResponseId = data.response_id;
                ratingForm.style.display = 'block';
                ratingThanks.style.display = 'none';
                ratingContainer.style.display = 'block';
            }
        });
    });

    // --- 5. 处理评分提交的逻辑 ---
    ratingForm.addEventListener('submit', function(event) {
        event.preventDefault();
        const selectedRating = ratingForm.querySelector('input[name="rating"]:checked');
        const comment = ratingForm.querySelector('#rating-comment').value;
        if (!selectedRating) return;

       const ratingApiUrl = 'https://ai-stats-book.onrender.com/api/rate-feedback';
        
        fetch(ratingApiUrl, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({
                response_id: currentResponseId,
                rating: parseInt(selectedRating.value, 10),
                comment: comment
            })
        })
        .then(response => {
            if (response.ok) {
                ratingForm.style.display = 'none';
                ratingThanks.style.display = 'block';
            }
        });
    });
});