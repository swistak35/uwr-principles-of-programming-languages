(module scheduler (lib "eopl.ss" "eopl")
  
  (require "drscheme-init.scm")
  (require "queues.scm")
  (require "data-structures.scm")       ; for continuation?
  (require "lang.scm")                  ; for expval?
  
  (provide
    initialize-scheduler!
    set-final-answer! 
    
    a-thread
    thread->thunk
    the-time-remaining
    the-max-time-slice
    time-expired?
    decrement-timer!

    place-on-ready-queue!
    run-next-thread

    
    )
  
  ;;;;;;;;;;;;;;;; the state ;;;;;;;;;;;;;;;;

  (define (anything? x) #t)
  
  (define-datatype thread thread?
    (a-thread
      (saved-thunk anything?)
      (rest-time integer?)))

  (define thread->thunk
    (lambda (v)
      (cases thread v
        (a-thread (saved-thunk rest-time) saved-thunk))))

  (define thread->rest-time
    (lambda (v)
      (cases thread v
        (a-thread (saved-thunk rest-time) rest-time))))


  ;; components of the scheduler state:
  
  (define the-ready-queue   'uninitialized)         
  (define the-final-answer  'uninitialized)
  
  (define the-max-time-slice    'uninitialized)
  (define the-time-remaining    'uninitialized)

  ;; initialize-scheduler! : Int -> Unspecified
  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice) 
      ))
  
  ;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

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

  )
