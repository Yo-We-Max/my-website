// DATA: Projects to display
const projects = [
    {
        title: "Portfolio Website",
        type: "web",
        description: "A responsive, single-page portfolio site built with HTML, CSS & JS.",
        tech: "HTML · CSS · JavaScript"
    },
    {
        title: "Pong Game",
        type: "games",
        description: "Classic Pong clone with keyboard controls and score tracking.",
        tech: "C · SDL2"
    },
    {
        title: "Snake Game",
        type: "games",
        description: "Retro snake with grid-based movement and growing tail.",
        tech: "Python · Pygame"
    },
    {
        title: "Task Tracker",
        type: "web",
        description: "Simple task manager with local storage persistence.",
        tech: "JavaScript · LocalStorage"
    }
];

// NAVIGATION BETWEEN SECTIONS
const navLinks = document.querySelectorAll(".nav-link");
const sections = document.querySelectorAll(".section");

navLinks.forEach((btn) => {
    btn.addEventListener("click", () => {
        const targetId = btn.dataset.section;

        // Update active nav button
        navLinks.forEach((b) => b.classList.remove("active"));
        btn.classList.add("active");

        // Show the right section
        sections.forEach((sec) => {
            sec.classList.toggle("active", sec.id === targetId);
        });
    });
});

// HERO BUTTON -> SCROLL TO PROJECTS + SWITCH SECTION
const scrollToProjectsBtn = document.getElementById("scrollToProjects");
scrollToProjectsBtn.addEventListener("click", () => {
    // Activate Projects tab
    const projectsNav = document.querySelector('.nav-link[data-section="projects"]');
    projectsNav.click();

    // Smooth scroll
    document.getElementById("projects").scrollIntoView({ behavior: "smooth" });
});

// RENDER PROJECTS
const projectsListEl = document.getElementById("projectsList");
const filterButtons = document.querySelectorAll(".filter-btn");

function renderProjects(filter = "all") {
    projectsListEl.innerHTML = "";

    const visibleProjects =
        filter === "all"
            ? projects
            : projects.filter((project) => project.type === filter);

    if (visibleProjects.length === 0) {
        projectsListEl.innerHTML =
            '<p style="color:#9ca3af;">No projects found for this category yet.</p>';
        return;
    }

    visibleProjects.forEach((project) => {
        const card = document.createElement("article");
        card.className = "project-card";

        card.innerHTML = `
      <div class="project-tag">${project.type.toUpperCase()}</div>
      <h3 class="project-title">${project.title}</h3>
      <p class="project-desc">${project.description}</p>
      <div class="project-meta">
        <span>${project.tech}</span>
        <span>⭐️ Demo only</span>
      </div>
    `;

        projectsListEl.appendChild(card);
    });
}

filterButtons.forEach((btn) => {
    btn.addEventListener("click", () => {
        filterButtons.forEach((b) => b.classList.remove("active"));
        btn.classList.add("active");
        const filter = btn.dataset.filter;
        renderProjects(filter);
    });
});

// INITIAL RENDER
renderProjects("all");

// CONTACT FORM VALIDATION + FAKE SUBMIT
const contactForm = document.getElementById("contactForm");
const formStatus = document.getElementById("formStatus");

function showError(inputId, message) {
    const span = document.querySelector(`.error[data-for="${inputId}"]`);
    if (span) span.textContent = message;
}

function clearErrors() {
    document.querySelectorAll(".error").forEach((span) => {
        span.textContent = "";
    });
    formStatus.textContent = "";
}

contactForm.addEventListener("submit", (e) => {
    e.preventDefault();
    clearErrors();

    const name = contactForm.name.value.trim();
    const email = contactForm.email.value.trim();
    const message = contactForm.message.value.trim();

    let hasError = false;

    if (!name) {
        showError("name", "Please enter your name.");
        hasError = true;
    }

    if (!email) {
        showError("email", "Please enter your email.");
        hasError = true;
    } else if (!/^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(email)) {
        showError("email", "Please enter a valid email address.");
        hasError = true;
    }

    if (!message) {
        showError("message", "Please enter a message.");
        hasError = true;
    }

    if (hasError) {
        formStatus.textContent = "Please fix the errors above and try again.";
        formStatus.style.color = "#f97373";
        return;
    }

    // Simulate sending the message (in reality you'd send this to a server)
    formStatus.textContent = "Message sent! I'll get back to you soon.";
    formStatus.style.color = "#4ade80";

    contactForm.reset();
});

// SET CURRENT YEAR IN FOOTER
document.getElementById("year").textContent = new Date().getFullYear();