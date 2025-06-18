## I. 🧭 Giao diện chính và chức năng

#### 1. **📊 Dashboard tổng quan**

Hiển thị thống kê nhanh cho toàn bộ dữ liệu TNGT:

-   **Lọc giới tính** và **độ tuổi**

-   🔝 **Top 10 mã chẩn đoán TNGT phổ biến** (ICD-10 V00-V99)

-    📊 **Biểu đồ số ngày nằm viện (LOS)** theo mã chẩn đoán

-   🔍 **Top 5 chẩn đoán kèm theo** (không phải mã TNGT)

-    📑 **Bảng các loại chấn thương chính thường gặp**

✅ *Tự động cập nhật khi thay đổi bộ lọc.*

#### 2. **📁 Xem dữ liệu**

Cho phép xem trực tiếp nội dung 3 bảng dữ liệu:

-    **Admission** – Thông tin nhập viện

-    **Diagnosis** – Danh sách mã chẩn đoán (ICD)

-    **ICD10** – Mô tả đầy đủ mã ICD-10

🔎 Có bộ lọc cột, có thể **xuất file Excel** từng bảng.

#### 3. **📁 Tải dữ liệu**

#### a. **Cập nhật dữ liệu**

-    Mỗi bảng có khu vực tải riêng:

    -    `Admission.xlsx` hoặc `.csv`

    -    `Diagnosis.xlsx` hoặc `.csv`

    -    `ICD10.xlsx` hoặc `.csv`

-   ➡️ **Khi file được chọn và upload**, hệ thống sẽ:

    -    Nạp dữ liệu mới

    -    Ghi đè `*.rds` trong thư mục `data/`

    -    Cập nhật toàn bộ biểu đồ và báo cáo tự động

#### b. **Tải dữ liệu**

-    Nút **“Tải toàn bộ dữ liệu”**: xuất gộp 3 bảng thành 1 file `.xlsx`

-    Nút **“Tải bảng hiện tại”**: chỉ tải bảng đang xem ở tab “Xem dữ liệu”

#### 4. **📈 Số ca TNGT theo tuổi & giới**

Phân tích số lượng ca TNGT từ độ tuổi 12–18:

-    Bộ lọc **Giới tính**

-    Biểu đồ cột phân bố theo **tuổi & giới**

-    Bảng tần suất **chẩn đoán chính** phân theo nhóm tuổi

-    **Kiểm định Chi-squared** giữa nhóm tuổi và loại chấn thương

📊 Biểu đồ sử dụng `ggplot2 + plotly`, tương tác xem chi tiết.

#### 5. **🚑 Phân tích Dữ liệu nhập viện**

**🏥 Tình trạng nhập viện trong các ca TNGT**

-    Biểu đồ tròn phân bố **nguồn nhập viện**

-    Các nguồn: A - từ nhà riêng, B - nơi khác, H - viện khác, N - viện dưỡng lão...

**💊 Số ngày điều trị trung bình theo từng loại nhập việ**n

Có thể quan sát số lượng và tỷ lệ từng nhóm.

#### 6. **🩺 Phân tích Kết quả điều trị**

Biểu đồ tròn thể hiện **tình trạng ra viện**, gồm:

-   A: Về nhà

-   B: Chuyển viện

-    D: Tử vong

-    H/N/S/T: Khác

#### 8. **ℹ️ Thông tin số liệu**

Hiển thị:

-    📋 Tổng quan về quy trình mã hoá

-    ✅ Cấu trúc bảng dữ liệu

-    📑 Nguồn dữ liệu và đơn vị thu thập

📌 Có thể nhúng nội dung từ `README.md` hoặc một phần phân tích được markdown hóa để trình bày tại đây.

## II. 📦 Quy trình cập nhật & tái tạo báo cáo

| Bước | Mô tả                                                         |
|------|---------------------------------------------------------------|
| 1️⃣   | Vào **tab “Tải dữ liệu”**                                     |
| 2️⃣   | Upload các bảng mới (`.xlsx` hoặc `.csv`)                     |
| 3️⃣   | Chuyển sang các tab phân tích (`Dashboard`, `Số ca TNGT`,...) |
| 4️⃣   | Tùy chọn lọc theo giới, tuổi...                               |
| 5️⃣   | Xuất file báo cáo Excel hoặc chụp màn hình biểu đồ            |
