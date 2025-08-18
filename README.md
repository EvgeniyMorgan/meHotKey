> [!IMPORTANT]
> Most likely, there won’t be full documentation—just no time for it, unfortunately((.<br>
> For any potential issues, please email us. ewgen.morgunov@yandex.ru

### **Component Description: TmeHotKey**  

`TmeHotKey` is a specialized edit control designed for capturing and displaying keyboard shortcuts (hotkeys) in Lazarus applications. It provides a user-friendly way to set and manage key combinations (e.g., `Ctrl+S`, `Alt+F4`, `Win+R`).  

---

![image](/images/preview.png "preview")

---

### **Key Features:**  
**Hotkey Capture** – Detects and displays pressed key combinations in real time.  
**Modifier Keys Support** – Works with `Ctrl`, `Alt`, `Shift`, and `Win` (Windows key).  
**Special Key Handling** – Properly displays function keys (`F1-F24`), navigation keys (`Home`, `End`, `PgUp`, etc.), and NumPad symbols (`Num +`, `Num -`, `Num *`, `Num /`).  
**Customizable** – Allows enabling/disabling the Windows key (`WinKey` property).  
**Event-Driven** – Triggers `OnExecute` when a valid hotkey is set.  
**Visual Feedback** – Shows the current hotkey in a readable format (e.g., `Ctrl+Shift+Del`).  

---

### **Properties:**  
| Property      | Type           | Description | Default Value |  
|--------------|----------------|-------------|---------------|  
| `ShortCut`   | `TShortCut`    | The currently assigned hotkey (e.g., `Ctrl+A`). | `Ctrl+A` |  
| `Active`     | `Boolean`      | Enables/disables hotkey processing. | `True` |  
| `WinKey`     | `Boolean`      | Allows/disallows the Windows key (`Win`) in shortcuts. | `False` |  
| `Description`| `string`       | Optional description for the hotkey (e.g., "Save File"). | Empty |  

**Inherited from `TCustomEdit`:**  
Standard edit control properties like `Text`, `ReadOnly`, `Font`, `Color`, etc.  

---

### **Methods:**  
| Method | Description |  
|--------|-------------|  
| `ShortCutToText()` | Converts a `TShortCut` value to a human-readable string (e.g., `Ctrl+A`). |  
| `TextToShortCut()` | Converts a string (e.g., `"Ctrl+A"`) back to a `TShortCut` value. |  
| `UpdateDisplay()` | Refreshes the displayed hotkey text. |  

---

### **Events:**  
| Event | Description |  
|-------|-------------|  
| `OnExecute` | Fires when a valid hotkey is set (if `Active=True`). |  

---

### **Usage Example:**  
```pascal
// Set a hotkey programmatically
meHotKey1.ShortCut := Menus.ShortCut(Ord('S'), [ssCtrl]);

// Handle hotkey assignment
procedure TForm1.meHotKey1Execute(Sender: TObject);
begin
  ShowMessage('Hotkey set: ' + TmeHotKey(Sender).ShortCutToText(TmeHotKey(Sender).ShortCut));
end;
```

---

### **Notes:**  
- Supports both standard and extended (NumPad) keys.  
- Prevents invalid key combinations (e.g., standalone modifier keys like `Shift`).  
- Ideal for settings panels, keyboard shortcut managers, or any app requiring customizable hotkeys.  

**Component Name:** `TmeHotKey`  
**Unit:** `meHotKey.pas`  
**Inheritance:** `TCustomEdit` → `TmeHotKey`  
