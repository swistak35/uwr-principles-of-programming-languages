(module scheduler (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "queues.scm")
  (require "data-structures.scm")       ; for continuation?
  (require "lang.scm")                  ; for expval?

  (require (only-in racket/base
                    filter))
  
  (provide
    initialize-scheduler!
    set-final-answer! 
    
    a-thread
    thread->thunk
    current-thread-id
    get-next-thread-id!
    the-time-remaining
    the-max-time-slice
    time-expired?
    decrement-timer!

    place-on-ready-queue!
    run-next-thread

    kill-threads
    
    )
  
  ;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;

  (define (anything? x) #t)
  
  (define-datatype thread thread?
    (a-thread
      (id integer?)
      (saved-thunk anything?)
      (rest-time integer?)))

  (define thread->id
    (lambda (v)
      (cases thread v
             (a-thread (id saved-thunk rest-time) id))))

  (define thread->thunk
    (lambda (v)
      (cases thread v
        (a-thread (id saved-thunk rest-time) saved-thunk))))

  (define thread->rest-time
    (lambda (v)
      (cases thread v
        (a-thread (id saved-thunk rest-time) rest-time))))


  ;; components of the scheduler state:
  
  (define the-ready-queue   'uninitialized)         
  (define the-final-answer  'uninitialized)
  
  (define the-max-time-slice    'uninitialized)
  (define the-time-remaining    'uninitialized)

  (define the-thread-counter 'uninitialized)
  (define current-thread-id 'uninitialized)

  ;; initialize-scheduler! : Int -> Unspecified
  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice) 
      (set! the-thread-counter 0)
      (set! current-thread-id 0)
      ))
  
  ;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

  (define (get-next-thread-id!)
    (set! the-thread-counter (+ the-thread-counter 1))
    the-thread-counter)
        
  ;; place-on-ready-queue! : Thread -> Unspecified
  ;; Page: 184  
  (define place-on-ready-queue!
    (lambda (th)
      (set! the-ready-queue
        (enqueue the-ready-queue th))))

  ;; run-next-thread : () -> FinalAnswer
  ;; Page: 184    
  (define run-next-thread
    (lambda ()
      (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
          (lambda (first-ready-thread other-ready-threads)
            (set! the-ready-queue other-ready-threads)            
            (set! the-time-remaining (thread->rest-time first-ready-thread))
            (set! current-thread-id (thread->id first-ready-thread))
            ((thread->thunk first-ready-thread))
            )))))

  ;; set-final-answer! : ExpVal -> Unspecified
  ;; Page: 184    
  (define set-final-answer!
    (lambda (val)
      (set! the-final-answer val)))

  ;; time-expired? : () -> Bool
  ;; Page: 184    
  (define time-expired?
    (lambda ()
      (zero? the-time-remaining)))

  ;; decrement-timer! : () -> Unspecified
  ;; Page: 184    
  (define decrement-timer!
    (lambda ()
	  ; (eopl:printf "D")
      (set! the-time-remaining (- the-time-remaining 1))))

  (define (kill-threads kill-id)
    (let* ((new-ready-queue
             (filter (lambda (th) (not (eq? kill-id (thread->id th)))) the-ready-queue))
           (ready-queue-changed? (not (eq? (length new-ready-queue) (length the-ready-queue)))))
      ; (pretty-display new-ready-queue)
      ; (pretty-display the-ready-queue)
      (set! the-ready-queue new-ready-queue)
      ready-queue-changed?))

  ; (trace kill-threads)

  )
