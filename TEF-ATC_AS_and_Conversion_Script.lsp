;; Smart AutoCAD Object Management Script
;; Menu-driven script with different options:
;; - Telefonica-AS: Delete objects on demolition layers + color operations
;; - ATC-AS: Delete demolition objects + move 10_/50_ objects to 10B_/50B_ + color operations + set plot colors
;; - Convert Telefonica to ATC: Move specific Telefonica objects to 50B_ layers

(defun c:TELEFONICA-AS ()
  (setq *error* my-error-handler)
  (princ "\n--- TELEFONICA-AS Operation ---")
  (princ "\nDeleting objects on demolition layers only...")
  
  ;; Delete objects on demolition layers
  (delete-objects-on-layer "00_Demontage")
  (delete-objects-on-layer "00_Demontage_Text")
  
  ;; Color-based operations
  (princ "\n\nStep 2: Processing color-based operations...")
  (change-color-on-layers (list "60_" "70_" "80_") '(0 130 140) '(0 0 0))
  (delete-objects-by-color-on-layers (list "50B_") '(165 82 0))
  
  (princ "\n--- TELEFONICA-AS Operation Complete ---")
  (princ "\nUse REGEN command to refresh the display if needed.")
  (princ)
)

(defun c:ATC-AS ()
  (setq *error* my-error-handler)
  (princ "\n--- ATC-AS Operation ---")
  (princ "\nPerforming full layer management...")
  
  ;; Step 1: Delete objects on demolition layers
  (princ "\n\nStep 1: Deleting objects on demolition layers...")
  (delete-objects-on-layer "00_Demontage")
  (delete-objects-on-layer "00_Demontage_Text")
  
  ;; Step 2: Move objects from 10_ layers to 10B_
  (princ "\n\nStep 2: Moving objects from '10_*' layers to '10B_*' layers...")
  (setq layers-to-process (get-layers-with-prefix "10_"))
  
  (if layers-to-process
    (progn
      (princ (strcat "\nFound " (itoa (length layers-to-process)) " layers starting with '10_'"))
      (foreach source-layer layers-to-process
        (setq target-layer (strcat "10B_" (substr source-layer 4)))
        (move-objects-to-layer source-layer target-layer)
      )
    )
    (princ "\nNo layers found starting with '10_'")
  )
  
  ;; Step 3: Move objects from 50_ layers to 50B_
  (princ "\n\nStep 3: Moving objects from '50_*' layers to '50B_*' layers...")
  (setq layers-to-process-50 (get-layers-with-prefix "50_"))
  
  (if layers-to-process-50
    (progn
      (princ (strcat "\nFound " (itoa (length layers-to-process-50)) " layers starting with '50_'"))
      (foreach source-layer layers-to-process-50
        (setq target-layer (strcat "50B_" (substr source-layer 4)))
        (move-objects-to-layer source-layer target-layer)
      )
    )
    (princ "\nNo layers found starting with '50_'")
  )
  
  ;; Step 4: Color-based operations
  (princ "\n\nStep 4: Processing color-based operations...")
  (change-color-on-layers (list "60_" "70_" "80_") '(0 130 140) '(0 0 0))
  (delete-objects-by-color-on-layers (list "50B_") '(165 82 0))
  
  ;; Step 5: Set plot colors to ACI 7 (black/white) for specific layer prefixes
  (princ "\n\nStep 5: Setting plot colors to ACI 7 (black/white)...")
  (set-objects-color-to-aci (list "10B_" "50B_" "60_" "70_" "80_" "90_") 7)
  
  (princ "\n--- ATC-AS Operation Complete ---")
  (princ "\nUse REGEN command to refresh the display if needed.")
  (princ)
)

(defun c:CONVERT-TEL-TO-ATC ()
  (setq *error* my-error-handler)
  (princ "\n--- Convert Telefonica to ATC Operation ---")
  (princ "\nConverting Telefonica objects to ATC format...")
  
  ;; Define the source layer patterns to search for
  (setq telefonica-patterns (list "10_Antennentraeger" "10_SystemGestell" "10_Zuwegung" 
                                  "10_Mast" "10_Fundament" "10B_Antennentraeger" 
                                  "10B_SystemGestell" "10B_Zuwegung" "10B_Mast" 
                                  "10B_Fundament" "20_Antennentraeger" "20_SystemGestell" 
                                  "20_Zuwegung" "20_Mast" "20_Fundament"))
  
  (setq total-moved 0)
  
  ;; Process each pattern
  (foreach pattern telefonica-patterns
    (princ (strcat "\n\nProcessing pattern: " pattern "*"))
    
    ;; Get all layers that match this pattern
    (setq matching-layers (get-layers-with-pattern pattern))
    
    (if matching-layers
      (progn
        (princ (strcat "\nFound " (itoa (length matching-layers)) " matching layers"))
        (foreach source-layer matching-layers
          ;; Determine target layer name
          (setq target-layer (convert-to-50b-layer source-layer pattern))
          
          ;; Move objects from source to target layer
          (setq moved-count (move-objects-to-layer-with-count source-layer target-layer))
          (setq total-moved (+ total-moved moved-count))
        )
      )
      (princ (strcat "\nNo layers found matching pattern: " pattern "*"))
    )
  )
  
  (princ (strcat "\n\nTotal objects moved: " (itoa total-moved)))
  (princ "\n--- Convert Telefonica to ATC Operation Complete ---")
  (princ "\nUse REGEN command to refresh the display if needed.")
  (princ)
)

(defun c:CONVERT-OLD-TEF-TO-NEW-TEF ()
  (setq *error* my-error-handler)
  (princ "\n--- Convert Old TEF to New TEF Operation ---")
  (princ "\nConverting old Telefonica format to new format...")
  (princ "\nMoving all objects from 20_* layers to 10B_* layers...")
  
  ;; Get all layers starting with "20_"
  (setq source-layers (get-layers-with-prefix "20_"))
  (setq total-moved 0)
  
  (if source-layers
    (progn
      (princ (strcat "\nFound " (itoa (length source-layers)) " layers starting with '20_'"))
      (foreach source-layer source-layers
        ;; Convert 20_ to 10B_ (e.g., "20_Antennentraeger" -> "10B_Antennentraeger")
        (setq target-layer (strcat "10B_" (substr source-layer 4)))
        
        ;; Move objects from source to target layer
        (setq moved-count (move-objects-to-layer-with-count source-layer target-layer))
        (setq total-moved (+ total-moved moved-count))
      )
    )
    (princ "\nNo layers found starting with '20_'")
  )
  
  (princ (strcat "\n\nTotal objects moved: " (itoa total-moved)))
  (princ "\n--- Convert Old TEF to New TEF Operation Complete ---")
  (princ "\nUse REGEN command to refresh the display if needed.")
  (princ)
)

(defun c:SET-TELEFONICA-PLOTSTYLE ()
  (setq *error* my-error-handler)
  (princ "\n--- Set Telefonica Plot Style Operation ---")
  (princ "\nSetting plot style to 'Plotstil Telefonica GU_v5.19.stb' for all paper space tabs...")
  
  (setq plotstyle-name "Plotstil Telefonica GU_v5.19.stb")
  (setq tabs-updated 0)
  (setq current-tab (getvar "CTAB"))
  
  ;; Get all layout names (paper space tabs)
  (setq layout-list (get-all-layouts))
  
  (if layout-list
    (progn
      (princ (strcat "\nFound " (itoa (length layout-list)) " paper space layouts"))
      (foreach layout-name layout-list
        (princ (strcat "\nProcessing layout: " layout-name))
        
        ;; Switch to the layout
        (setvar "CTAB" layout-name)
        
        ;; Set the plot style table for this layout
        (if (set-layout-plotstyle layout-name plotstyle-name)
          (progn
            (setq tabs-updated (1+ tabs-updated))
            (princ (strcat " - Plot style updated successfully"))
          )
          (princ (strcat " - Warning: Could not update plot style"))
        )
      )
      
      ;; Return to original tab
      (setvar "CTAB" current-tab)
      
      (princ (strcat "\n\nSuccessfully updated " (itoa tabs-updated) " paper space layouts"))
    )
    (princ "\nNo paper space layouts found")
  )
  
  (princ "\n--- Set Telefonica Plot Style Operation Complete ---")
  (princ)
)

;; Updated LAYERMANAGE command with new option
(defun c:LAYERMANAGE ()
  (princ "\nWhich operation would you like to perform?")
  (princ "\n1. TELEFONICA-AS - Delete demolition objects only")
  (princ "\n2. ATC-AS - Full layer management (delete + move objects + set plot colors)")
  (princ "\n3. CONVERT-TEL-TO-ATC - Convert Telefonica to ATC format")
  (princ "\n4. CONVERT-OLD-TEF-TO-NEW-TEF - Convert old TEF to new TEF (20_* to 10B_*)")
  (princ "\n5. SET-TELEFONICA-PLOTSTYLE - Set Telefonica plot style for all paper space tabs")
  (princ "\nEnter 1, 2, 3, 4, or 5: ")
  (setq choice (getstring))
  (cond
    ((= choice "1") (c:TELEFONICA-AS))
    ((= choice "2") (c:ATC-AS))
    ((= choice "3") (c:CONVERT-TEL-TO-ATC))
    ((= choice "4") (c:CONVERT-OLD-TEF-TO-NEW-TEF))
    ((= choice "5") (c:SET-TELEFONICA-PLOTSTYLE))
    (t (princ "\nInvalid choice. Operation cancelled."))
  )
  (princ)
)

;; Function to handle errors
(defun my-error-handler (msg)
  (if (/= msg "Function cancelled")
    (princ (strcat "\nError: " msg))
  )
  (setq *error* nil)
  (princ)
)

;; Function to check if layer exists
(defun layer-exists-p (layer-name)
  (if (tblsearch "LAYER" layer-name)
    T
    nil
  )
)

;; Function to delete all objects on a specific layer
(defun delete-objects-on-layer (layer-name)
  (if (layer-exists-p layer-name)
    (progn
      (setq obj-count 0)
      (setq ss (ssget "X" (list (cons 8 layer-name))))
      (if ss
        (progn
          (setq obj-count (sslength ss))
          (setq i 0)
          (while (< i obj-count)
            (setq ent (ssname ss i))
            (entdel ent)
            (setq i (1+ i))
          )
          (princ (strcat "\nDeleted " (itoa obj-count) " objects from layer: " layer-name))
        )
        (princ (strcat "\nNo objects found on layer: " layer-name))
      )
    )
    (princ (strcat "\nLayer '" layer-name "' not found."))
  )
)

;; Function to create a layer if it doesn't exist
(defun create-layer-if-missing (layer-name)
  (if (not (layer-exists-p layer-name))
    (progn
      (setq layer-table (tblnext "LAYER" T))
      (setq new-layer-data (list (cons 0 "LAYER")
                                (cons 100 "AcDbSymbolTableRecord")
                                (cons 100 "AcDbLayerTableRecord")
                                (cons 2 layer-name)
                                (cons 70 0)
                                (cons 62 7)  ; Color 7 (white)
                                (cons 6 "CONTINUOUS")))
      (setq result (vl-catch-all-apply 'entmakex (list new-layer-data)))
      (if (vl-catch-all-error-p result)
        (princ (strcat "\nWarning: Could not create layer '" layer-name "'"))
        (princ (strcat "\nCreated layer: " layer-name))
      )
    )
  )
)

;; Function to move objects from one layer to another
(defun move-objects-to-layer (from-layer to-layer)
  (if (layer-exists-p from-layer)
    (progn
      ;; Create target layer if it doesn't exist
      (create-layer-if-missing to-layer)
      
      (setq obj-count 0)
      (setq ss (ssget "X" (list (cons 8 from-layer))))
      (if ss
        (progn
          (setq obj-count (sslength ss))
          (setq i 0)
          (while (< i obj-count)
            (setq ent (ssname ss i))
            (setq ent-data (entget ent))
            ;; Change the layer property (DXF code 8)
            (setq ent-data (subst (cons 8 to-layer) (assoc 8 ent-data) ent-data))
            (entmod ent-data)
            (setq i (1+ i))
          )
          (princ (strcat "\nMoved " (itoa obj-count) " objects from layer '" from-layer "' to '" to-layer "'"))
        )
        (princ (strcat "\nNo objects found on layer: " from-layer))
      )
    )
    (princ (strcat "\nSource layer '" from-layer "' not found."))
  )
)

;; Function to move objects and return count moved
(defun move-objects-to-layer-with-count (from-layer to-layer)
  (setq obj-count 0)
  (if (layer-exists-p from-layer)
    (progn
      ;; Create target layer if it doesn't exist
      (create-layer-if-missing to-layer)
      
      (setq ss (ssget "X" (list (cons 8 from-layer))))
      (if ss
        (progn
          (setq obj-count (sslength ss))
          (setq i 0)
          (while (< i obj-count)
            (setq ent (ssname ss i))
            (setq ent-data (entget ent))
            ;; Change the layer property (DXF code 8)
            (setq ent-data (subst (cons 8 to-layer) (assoc 8 ent-data) ent-data))
            (entmod ent-data)
            (setq i (1+ i))
          )
          (princ (strcat "\nMoved " (itoa obj-count) " objects from layer '" from-layer "' to '" to-layer "'"))
        )
        (princ (strcat "\nNo objects found on layer: " from-layer))
      )
    )
    (princ (strcat "\nSource layer '" from-layer "' not found."))
  )
  obj-count
)

;; Function to get all layer names starting with a prefix
(defun get-layers-with-prefix (prefix)
  (setq layer-list '())
  (setq layer-table (tblnext "LAYER" T))
  (while layer-table
    (setq layer-name (cdr (assoc 2 layer-table)))
    (if (= (substr layer-name 1 (strlen prefix)) prefix)
      (setq layer-list (cons layer-name layer-list))
    )
    (setq layer-table (tblnext "LAYER"))
  )
  layer-list
)

;; Function to get all layer names that match a pattern (starts with pattern)
(defun get-layers-with-pattern (pattern)
  (setq layer-list '())
  (setq layer-table (tblnext "LAYER" T))
  (while layer-table
    (setq layer-name (cdr (assoc 2 layer-table)))
    ;; Check if layer name starts with the pattern
    ;; This will match: 10_SystemGestell, 10_SystemGestell01, 10_SystemGestell_VDK, etc.
    (if (and (>= (strlen layer-name) (strlen pattern))
             (= (substr layer-name 1 (strlen pattern)) pattern))
      (setq layer-list (cons layer-name layer-list))
    )
    (setq layer-table (tblnext "LAYER"))
  )
  layer-list
)

;; Function to convert source layer name to 50B_ target layer
(defun convert-to-50b-layer (source-layer pattern)
  (cond
    ;; For 10_ layers
    ((= (substr pattern 1 3) "10_")
     ;; Get everything after the pattern
     (setq suffix (substr source-layer (+ (strlen pattern) 1)))
     ;; Create 50B_ layer name: 50B_ + pattern without "10_" + suffix
     (strcat "50B_" (substr pattern 4) suffix)
    )
    ;; For 10B_ layers
    ((= (substr pattern 1 4) "10B_")
     ;; Get everything after the pattern
     (setq suffix (substr source-layer (+ (strlen pattern) 1)))
     ;; Create 50B_ layer name: 50B_ + pattern without "10B_" + suffix
     (strcat "50B_" (substr pattern 5) suffix)
    )
    ;; For 20_ layers
    ((= (substr pattern 1 3) "20_")
     ;; Get everything after the pattern
     (setq suffix (substr source-layer (+ (strlen pattern) 1)))
     ;; Create 50B_ layer name: 50B_ + pattern without "20_" + suffix
     (strcat "50B_" (substr pattern 4) suffix)
    )
    ;; Default case - shouldn't happen with our predefined patterns
    (t (strcat "50B_" source-layer))
  )
)

;; Function to convert RGB color to AutoCAD color index (approximate)
(defun rgb-to-autocad-color (rgb-list)
  ;; This is a simplified conversion - AutoCAD uses ACI (AutoCAD Color Index)
  ;; For precise color matching, we'll use RGB values directly in true color
  (setq r (nth 0 rgb-list))
  (setq g (nth 1 rgb-list))
  (setq b (nth 2 rgb-list))
  ;; Return true color value (RGB * 65536 + RGB * 256 + RGB)
  (+ (* r 65536) (* g 256) b)
)

;; Function to check if object has specific RGB color
(defun object-has-rgb-color (ent-data rgb-list)
  (setq color-code (cdr (assoc 62 ent-data)))  ; ACI color
  (setq true-color (cdr (assoc 420 ent-data))) ; True color
  
  (setq target-color (rgb-to-autocad-color rgb-list))
  
  ;; Check both ACI and true color
  (or (and true-color (= true-color target-color))
      ;; For common colors, check ACI equivalents
      (and (equal rgb-list '(0 130 140)) (or (= color-code 4) (= color-code 6)))
      (and (equal rgb-list '(165 82 0)) (= color-code 30))
      (and (equal rgb-list '(0 0 0)) (= color-code 7))
  )
)

;; Function to change color of objects on specified layers
(defun change-color-on-layers (layer-prefixes from-rgb to-rgb)
  (setq total-changed 0)
  (foreach prefix layer-prefixes
    (setq matching-layers (get-layers-with-prefix prefix))
    (foreach layer-name matching-layers
      (if (layer-exists-p layer-name)
        (progn
          (setq layer-changed 0)
          (setq ss (ssget "X" (list (cons 8 layer-name))))
          (if ss
            (progn
              (setq i 0)
              (while (< i (sslength ss))
                (setq ent (ssname ss i))
                (setq ent-data (entget ent))
                
                ;; Check if object has the target color
                (if (object-has-rgb-color ent-data from-rgb)
                  (progn
                    ;; Change to new color using true color
                    (setq new-color (rgb-to-autocad-color to-rgb))
                    (setq ent-data (subst (cons 420 new-color) (assoc 420 ent-data) ent-data))
                    ;; If no true color existed, add it
                    (if (not (assoc 420 ent-data))
                      (setq ent-data (cons (cons 420 new-color) ent-data))
                    )
                    ;; Remove ACI color to use true color
                    (if (assoc 62 ent-data)
                      (setq ent-data (vl-remove (assoc 62 ent-data) ent-data))
                    )
                    (entmod ent-data)
                    (setq layer-changed (1+ layer-changed))
                    (setq total-changed (1+ total-changed))
                  )
                )
                (setq i (1+ i))
              )
              (if (> layer-changed 0)
                (princ (strcat "\nChanged color of " (itoa layer-changed) " objects on layer: " layer-name))
              )
            )
          )
        )
      )
    )
  )
  (princ (strcat "\nTotal objects with color changed: " (itoa total-changed)))
)

;; Function to delete objects with specific color on specified layers
(defun delete-objects-by-color-on-layers (layer-prefixes rgb-color)
  (setq total-deleted 0)
  (foreach prefix layer-prefixes
    (setq matching-layers (get-layers-with-prefix prefix))
    (foreach layer-name matching-layers
      (if (layer-exists-p layer-name)
        (progn
          (setq layer-deleted 0)
          (setq ss (ssget "X" (list (cons 8 layer-name))))
          (if ss
            (progn
              (setq i 0)
              (while (< i (sslength ss))
                (setq ent (ssname ss i))
                (setq ent-data (entget ent))
                
                ;; Check if object has the target color
                (if (object-has-rgb-color ent-data rgb-color)
                  (progn
                    (entdel ent)
                    (setq layer-deleted (1+ layer-deleted))
                    (setq total-deleted (1+ total-deleted))
                  )
                )
                (setq i (1+ i))
              )
              (if (> layer-deleted 0)
                (princ (strcat "\nDeleted " (itoa layer-deleted) " objects with specific color on layer: " layer-name))
              )
            )
          )
        )
      )
    )
  )
  (princ (strcat "\nTotal objects deleted by color: " (itoa total-deleted)))
)

;; NEW FUNCTION: Set objects color to specific ACI color on layers with given prefixes
(defun set-objects-color-to-aci (layer-prefixes aci-color)
  (setq total-changed 0)
  (princ (strcat "\nSetting objects to ACI color " (itoa aci-color) " on layers with prefixes: " (vl-prin1-to-string layer-prefixes)))
  
  (foreach prefix layer-prefixes
    (setq matching-layers (get-layers-with-prefix prefix))
    (if matching-layers
      (progn
        (princ (strcat "\n  Processing " (itoa (length matching-layers)) " layers with prefix '" prefix "'"))
        (foreach layer-name matching-layers
          (if (layer-exists-p layer-name)
            (progn
              (setq layer-changed 0)
              (setq ss (ssget "X" (list (cons 8 layer-name))))
              (if ss
                (progn
                  (setq i 0)
                  (while (< i (sslength ss))
                    (setq ent (ssname ss i))
                    (setq ent-data (entget ent))
                    
                    ;; Remove true color if it exists (DXF code 420)
                    (if (assoc 420 ent-data)
                      (setq ent-data (vl-remove (assoc 420 ent-data) ent-data))
                    )
                    
                    ;; Set or update ACI color (DXF code 62)
                    (if (assoc 62 ent-data)
                      (setq ent-data (subst (cons 62 aci-color) (assoc 62 ent-data) ent-data))
                      (setq ent-data (cons (cons 62 aci-color) ent-data))
                    )
                    
                    (entmod ent-data)
                    (setq layer-changed (1+ layer-changed))
                    (setq total-changed (1+ total-changed))
                    (setq i (1+ i))
                  )
                  (if (> layer-changed 0)
                    (princ (strcat "\n    Set color for " (itoa layer-changed) " objects on layer: " layer-name))
                  )
                )
                (princ (strcat "\n    No objects found on layer: " layer-name))
              )
            )
            (princ (strcat "\n    Layer not found: " layer-name))
          )
        )
      )
      (princ (strcat "\n  No layers found with prefix: " prefix))
    )
  )
  (princ (strcat "\nTotal objects with color set to ACI " (itoa aci-color) ": " (itoa total-changed)))
)

;; Function to get all layout names (paper space tabs)
(defun get-all-layouts ()
  (setq layout-list '())
  (setq layout-dict (namedobjdict))
  (setq layout-entry (dictsearch layout-dict "ACAD_LAYOUT"))
  
  (if layout-entry
    (progn
      (setq layout-dict-obj (cdr (assoc -1 layout-entry)))
      (setq layout-names (dictnext layout-dict-obj T))
      
      (while layout-names
        (setq layout-name (car layout-names))
        ;; Skip "Model" as it's model space, not paper space
        (if (/= layout-name "Model")
          (setq layout-list (cons layout-name layout-list))
        )
        (setq layout-names (dictnext layout-dict-obj))
      )
    )
  )
  layout-list
)

;; Function to set plot style for a specific layout
(defun set-layout-plotstyle (layout-name plotstyle-name)
  (setq success nil)
  (setq layout-dict (namedobjdict))
  (setq layout-entry (dictsearch layout-dict "ACAD_LAYOUT"))
  
  (if layout-entry
    (progn
      (setq layout-dict-obj (cdr (assoc -1 layout-entry)))
      (setq layout-obj-entry (dictsearch layout-dict-obj layout-name))
      
      (if layout-obj-entry
        (progn
          (setq layout-obj (cdr (assoc -1 layout-obj-entry)))
          (setq layout-data (entget layout-obj))
          
          ;; Update the plot style table name (DXF code 7)
          (setq new-layout-data (subst (cons 7 plotstyle-name) 
                                       (assoc 7 layout-data) 
                                       layout-data))
          
          ;; If no plot style entry exists, add it
          (if (not (assoc 7 layout-data))
            (setq new-layout-data (cons (cons 7 plotstyle-name) new-layout-data))
          )
          
          ;; Apply the changes
          (if (entmod new-layout-data)
            (setq success T)
          )
        )
      )
    )
  )
  success
)

;; Load message
(princ "\nSmart Object Management Script Loaded!")
(princ "\nAvailable commands:")
(princ "\n  TELEFONICA-AS - Delete demolition objects only")
(princ "\n  ATC-AS - Full layer management + plot color setting")
(princ "\n  CONVERT-TEL-TO-ATC - Convert Telefonica to ATC format")
(princ "\n  CONVERT-OLD-TEF-TO-NEW-TEF - Convert old TEF to new TEF")
(princ "\n  SET-TELEFONICA-PLOTSTYLE - Set Telefonica plot style for all tabs")
(princ "\n  LAYERMANAGE - Interactive menu")
(princ)