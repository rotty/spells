(define-interface spells.srfi-19-interface
  (export time-duration ;; Constants
          time-monotonic
          time-process
          time-tai
          time-thread
          time-utc
          ;; Current time and clock resolution
          current-date
          current-julian-day
          current-modified-julian-day
          current-time
          time-resolution
          ;; Time object and accessors
          make-time
          time?
          time-type
          time-nanosecond
          time-second
          set-time-type!
          set-time-nanosecond!
          set-time-second!
          copy-time
          ;; Time comparison procedures
          time<=?
          time<?
          time=?
          time>=?
          time>?
          ;; Time arithmetic procedures
          time-difference
          time-difference!
          add-duration
          add-duration!
          subtract-duration
          subtract-duration!
          ;; Date object and accessors
          make-date
          date?
          date-nanosecond
          date-second
          date-minute
          date-hour
          date-day
          date-month
          date-year
          date-zone-offset
          date-year-day
          date-week-day
          date-week-number
          ;; Time/Date/Julian Day/Modified Julian Day converters
          date->julian-day
          date->modified-julian-day
          date->time-monotonic
          date->time-tai
          date->time-utc
          julian-day->date
          julian-day->time-monotonic
          julian-day->time-tai
          julian-day->time-utc
          modified-julian-day->date
          modified-julian-day->time-monotonic
          modified-julian-day->time-tai
          modified-julian-day->time-utc
          time-monotonic->date
          time-monotonic->time-tai
          time-monotonic->time-tai!
          time-monotonic->time-utc
          time-monotonic->time-utc!
          time-tai->date
          time-tai->julian-day
          time-tai->modified-julian-day
          time-tai->time-monotonic
          time-tai->time-monotonic!
          time-tai->time-utc
          time-tai->time-utc!
          time-utc->date
          time-utc->julian-day
          time-utc->modified-julian-day
          time-utc->time-monotonic
          time-utc->time-monotonic!
          time-utc->time-tai
          time-utc->time-tai!
          ;; Date to string/string to date converters.
          date->string
          string->date))

(define-interface spells.srfi-34-interface
  (export with-exception-handler
          raise
          ((guard)
           :syntax)))

(define-interface spells.srfi-35-interface
  (export make-condition-type
          condition-type?
          make-condition
          condition?
          condition-has-type?
          condition-ref
          make-compound-condition
          extract-condition
          ((define-condition-type
             condition)
           :syntax)
          &condition
          &message message-condition?
          condition-message
          &serious serious-condition?
          &error error?))

(define-interface spells.srfi-36-interface
  (export
   &i/o-error i/o-error?
   &i/o-port-error i/o-port-error?
   i/o-error-port
   &i/o-read-error i/o-read-error?
   &i/o-write-error i/o-write-error?))

(define-interface spells.condition-extras-interface
  (export &irritants irritants? condition-irritants
          &parser-error parser-error?
          parser-error-port
          &end-of-input end-of-input?
          end-of-input-port))

(define-interface spells.condition-interface
  (compound-interface spells.srfi-34-interface
                      spells.srfi-35-interface
                      spells.srfi-36-interface
                      spells.condition-extras-interface))

(define-interface spells.define-record-type-interface
  (export (define-record-type :syntax)
          define-record-discloser))

;; arch-tag: a4a455e7-5c3e-4157-b598-a531e44a9e78
