;;;; NEURAL-REASONER.LISP — A Symbolic AI Reasoning Engine
;;;; 100-line excerpt from a hybrid neural-symbolic inference system
;;;; Combines pattern matching, belief propagation, and chain-of-thought
;;;; reasoning over a knowledge graph. Written in Common Lisp.

;;; ============================================================
;;; KNOWLEDGE REPRESENTATION
;;; Beliefs are stored as (concept relation concept confidence)
;;; ============================================================

(defstruct belief
  subject predicate object (confidence 1.0) (source :axiom))

(defparameter *knowledge-base* (make-hash-table :test #'equal))
(defparameter *inference-chain* nil)
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *confidence-threshold* 0.3)
(defparameter *decay-factor* 0.85)

(defun store-belief (subj pred obj &optional (conf 1.0) (src :axiom))
  "Insert a belief into the knowledge base, indexed by subject."
  (let ((b (make-belief :subject subj :predicate pred
                        :object obj :confidence conf :source src)))
    (push b (gethash subj *knowledge-base* nil))
    b))

(defun query-beliefs (subj &optional pred)
  "Retrieve beliefs about SUBJ, optionally filtered by predicate."
  (let ((beliefs (gethash subj *knowledge-base*)))
    (if pred
        (remove-if-not (lambda (b) (eq (belief-predicate b) pred)) beliefs)
        beliefs)))

;;; ============================================================
;;; FORWARD-CHAINING INFERENCE ENGINE
;;; Fires rules to derive new beliefs from existing ones
;;; ============================================================

(defparameter *rules* nil)

(defstruct rule
  name antecedent-pred consequent-pred transform (weight 1.0))

(defun define-rule (name ante-pred cons-pred transform &optional (weight 1.0))
  "Register an inference rule: if (X ante-pred Y) then (Y cons-pred (transform X))."
  (push (make-rule :name name
                   :antecedent-pred ante-pred
                   :consequent-pred cons-pred
                   :transform transform
                   :weight weight)
        *rules*))

(defun fire-rules (subject &optional (depth 0) (max-depth 5))
  "Forward-chain from SUBJECT up to MAX-DEPTH with cycle detection."
  (when (>= depth max-depth) (return-from fire-rules nil))
  (let ((beliefs (gethash subject *knowledge-base*)))
    (dolist (belief beliefs)
      (dolist (rule *rules*)
        (when (eq (belief-predicate belief) (rule-antecedent-pred rule))
          (let* ((new-obj (funcall (rule-transform rule)
                                  (belief-subject belief)
                                  (belief-object belief)))
                 (new-conf (* (belief-confidence belief)
                              (rule-weight rule)
                              *decay-factor*))
                 (target (belief-object belief))
                 (visit-key (list target (rule-consequent-pred rule) new-obj)))
            (when (and (> new-conf *confidence-threshold*)
                       (not (gethash visit-key *visited*)))
              (setf (gethash visit-key *visited*) t)
              (let ((derived (store-belief target
                                          (rule-consequent-pred rule)
                                          new-obj new-conf
                                          (rule-name rule))))
                (push (list :depth depth :rule (rule-name rule)
                            :from subject :derived (belief-object derived)
                            :conf new-conf)
                      *inference-chain*)
                (fire-rules target (1+ depth) max-depth)))))))))

;;; ============================================================
;;; CHAIN-OF-THOUGHT REASONING — explain inference paths
;;; ============================================================

(defun explain-reasoning ()
  "Print the chain-of-thought trace for the last inference."
  (format t "~%=== CHAIN OF THOUGHT ===~%")
  (dolist (step (reverse *inference-chain*))
    (format t "  [depth ~A] ~12A : ~A -> ~A (conf: ~,3F)~%"
            (getf step :depth)
            (getf step :rule)
            (getf step :from)
            (getf step :derived)
            (getf step :conf)))
  (format t "========================~%"))

;;; ============================================================
;;; DEMO: Build a knowledge graph and reason over it
;;; ============================================================

(defun run-demo ()
  (setf *inference-chain* nil)
  (clrhash *knowledge-base*)
  (clrhash *visited*)
  (setf *rules* nil)

  ;; Seed knowledge: a small world model
  (store-belief :socrates  :is-a       :human)
  (store-belief :human     :is-a       :mortal)
  (store-belief :mortal    :has-prop   :dies)
  (store-belief :socrates  :knows      :philosophy  0.95)
  (store-belief :philosophy :enables   :reasoning   0.9)
  (store-belief :reasoning :enables    :inference   0.88)

  ;; Define inference rules
  (define-rule :transitivity :is-a :is-a
    (lambda (s o) (declare (ignore s)) o) 0.95)
  (define-rule :capability-propagation :enables :can-do
    (lambda (s o) (declare (ignore s)) o) 0.9)
  (define-rule :property-inheritance :has-prop :has-prop
    (lambda (s o) (declare (ignore s)) o) 0.8)

  ;; Run inference from Socrates
  (format t "~%Seeded knowledge base. Running inference...~%")
  (fire-rules :socrates)
  (fire-rules :human)
  (fire-rules :philosophy)

  ;; Show derived beliefs
  (format t "~%--- Beliefs about :HUMAN ---~%")
  (dolist (b (query-beliefs :human))
    (format t "  (~A ~A ~A) [conf: ~,3F, src: ~A]~%"
            (belief-subject b) (belief-predicate b) (belief-object b)
            (belief-confidence b) (belief-source b)))

  (format t "~%--- Beliefs about :MORTAL ---~%")
  (dolist (b (query-beliefs :mortal))
    (format t "  (~A ~A ~A) [conf: ~,3F, src: ~A]~%"
            (belief-subject b) (belief-predicate b) (belief-object b)
            (belief-confidence b) (belief-source b)))

  (explain-reasoning)

  ;; Query: Can reasoning lead to inference?
  (let ((results (query-beliefs :reasoning :can-do)))
    (format t "~%Query: Can reasoning lead to inference?~%")
    (if results
        (format t "  YES -- confidence: ~,3F~%"
                (belief-confidence (first results)))
        (format t "  No direct evidence found.~%"))))

(run-demo)
