; Windows-specific batch resize JPEG images script with alternative save method
(define (batch-resize-jpeg-windows input-dir output-dir percentage jpeg-quality)
  ; Print debug info
  (gimp-message "WINDOWS-SPECIFIC JPEG BATCH RESIZE")
  (gimp-message (string-append "Input directory: " input-dir))
  (gimp-message (string-append "Output directory: " output-dir))
  
  ; Try to create a test file in the output directory
  (let ((test-file (string-append output-dir "\\test_file.txt")))
    (gimp-message (string-append "Testing output directory with: " test-file))
    (let ((success #f))
      (catch
       (lambda (key . args)
         (gimp-message "Failed to write test file - check permissions"))
       (begin
         (let ((port (open-output-file test-file)))
           (if port
               (begin
                 (display "test" port)
                 (close-output-port port)
                 (set! success #t))))))
      
      (if (not success)
          (gimp-message "ERROR: Cannot write to output directory!")
          (gimp-message "Output directory is writable"))))
  
  ; Process images
  (let ((processed-count 0))
    ; Find JPEG files - try both .jpg and .jpeg
    (let* ((jpg-pattern1 (string-append input-dir "\\*.jpg"))
           ; (jpg-pattern2 (string-append input-dir "\\*.JPG"))
           (all-files (cadr (file-glob jpg-pattern1 1)))
           ; (files2 (cadr (file-glob jpg-pattern2 1)))
           ; (all-files (append files1 files2))
     )

      
      (gimp-message (string-append "Found " (number->string (length all-files)) " JPEG files"))
      
      ; Process each file
      (for-each
       (lambda (current-file)
         (gimp-message (string-append "Processing: " current-file))
         
         ; Extract filename - try both slash types
         (let* ((bname (if (> (length (strbreakup current-file "\\")) 1)
                          (car (last (strbreakup current-file "\\")))
                          (car (last (strbreakup current-file "/")))))
                (name-only "")
                (i 0)
                (dot-pos -1))
           
           ; Find the last dot position
           (while (< i (string-length bname))
             (if (equal? (substring bname i (+ i 1)) ".")
                 (set! dot-pos i))
             (set! i (+ i 1)))
           
           ; Get name without extension
           (if (> dot-pos 0)
               (set! name-only (substring bname 0 dot-pos))
               (set! name-only bname))
           
           ; Load the image
           (let ((image (car (gimp-file-load RUN-NONINTERACTIVE current-file current-file))))
             (when (> image 0)
               ; Get dimensions and resize
               (let* ((orig-width (car (gimp-image-width image)))
                      (orig-height (car (gimp-image-height image)))
                      (new-width (round (* orig-width (/ percentage 100))))
                      (new-height (round (* orig-height (/ percentage 100)))))
                 
                 (gimp-message (string-append "Resizing from " 
                                            (number->string orig-width) "x" (number->string orig-height)
                                            " to " 
                                            (number->string new-width) "x" (number->string new-height)))
                 
                 ; Resize the image
                 (gimp-image-scale image new-width new-height)
                 
                 ; Prepare for saving - try multiple output paths with different separators
                 (let* ((out-path1 (string-append output-dir "\\" name-only ".jpg"))
                        (out-path2 (string-append output-dir "/" name-only ".jpg"))
                        (drawable (car (gimp-image-get-active-layer image))))
                   
                   ; Flatten before saving
                   (gimp-image-flatten image)
                   (set! drawable (car (gimp-image-get-active-layer image)))
                   
                   ; Try save method 1 - file-jpeg-save with backslash path
                   (gimp-message (string-append "Trying save method 1: " out-path1))
                   (catch
                    (lambda (key . args)
                      (gimp-message "Method 1 save failed - trying method 2"))
                    (begin
                      (file-jpeg-save RUN-NONINTERACTIVE image drawable 
                                     out-path1 out-path1
                                     jpeg-quality 0 0 0 "" 0 0 0 0)))
                   
                   ; Try save method 2 - file-jpeg-save with forward slash path
                   ;(gimp-message (string-append "Trying save method 2: " out-path2))
                   (catch
                    (lambda (key . args)
                      (gimp-message "Method 2 save failed - trying method 3"))
                    (begin
                      (file-jpeg-save RUN-NONINTERACTIVE image drawable 
                                     out-path2 out-path2
                                     jpeg-quality 0 0 0 "" 0 0 0 0)))
                   
                   ; Try save method 3 - gimp-file-save with backslash path
                   ;(gimp-message (string-append "Trying save method 3: " out-path1))
                   (catch
                    (lambda (key . args)
                      (gimp-message "Method 3 save failed - trying method 4"))
                    (begin
                      (gimp-file-save RUN-NONINTERACTIVE image drawable 
                                     out-path1 out-path1)))
                   
                   ; Try save method 4 - gimp-file-save with forward slash path
                   ;(gimp-message (string-append "Trying save method 4: " out-path2))
                   (catch
                    (lambda (key . args)
                      (gimp-message "All save methods failed!"))
                    (begin
                      (gimp-file-save RUN-NONINTERACTIVE image drawable 
                                     out-path2 out-path2)
                      (set! processed-count (+ processed-count 1))))
                   
                   ; Clean up
                   (gimp-image-delete image)))))))
       all-files)
      
      ; Report completion
      (gimp-message (string-append "Batch processing complete. Processed: " 
                                  (number->string processed-count) " images")))))

; Register the script with GIMP
(script-fu-register
  "batch-resize-jpeg-windows"                ; func name
  "Batch Resize JPEG (Windows)"              ; menu label
  "Resize all JPEG images in a folder (Windows-specific version)" ; description
  "GitHub Copilot"                           ; author
  "Public Domain"                            ; copyright
  "2025"                                     ; date created
  ""                                         ; image type that the script works on
  SF-DIRNAME "Input Directory" ""
  SF-DIRNAME "Output Directory" ""
  SF-ADJUSTMENT "Resize percentage" '(50 1 1000 1 10 1 0)
  SF-ADJUSTMENT "JPEG quality" '(90 0 100 1 10 0 0)
)

(script-fu-menu-register "batch-resize-jpeg-windows" "<Image>/File/Batch")